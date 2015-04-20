(ns rksm.cloxp-trace.capturing
  (:require [clojure.zip :as z]
            [clojure.data.json :as json]
            [rksm.cloxp-trace.transform :as tfm]
            [rksm.cloxp-trace.source-mapping :refer [pos->ast-idx
                                                     with-source
                                                     indexed-expr-list]]
            [rksm.cloxp-source-reader.core :as src-rdr]
            [clojure.repl :as repl]
            [clojure.string :as s]
            [clj-stacktrace.core :as st]))

(def ^{:dynamic true} *repl-source*)

(defn type-of-def
  [form]
  (str (first form)))

(defn name-of-def
  [form]
  (first (drop 1 (filter symbol? form))))

(defn extract-defmethod-matches
  "takes a defmethod form and extract the match args from it, like
  '(defmethod ^{:dynamic true}foo-method String [::foo \"Bar\"] ([x] (.toUpperCase x)))
  =>
  '(String [:user/foo \"Bar\"])"
  [form]
  (let [ex-form (macroexpand form)
        [_ _ _ match-1 fn-def] ex-form
        rest-matches (if (= (->> fn-def last (map type))
                            [clojure.lang.PersistentVector clojure.lang.PersistentList])
                       (->> fn-def (drop 1) (drop-last))
                       (->> fn-def (drop 1) (drop-last 2)))]
    (cons match-1 rest-matches)))

(defmulti make-id (fn [type & _] type))

(defmethod make-id "defmethod"
  [type form ns name idx]
  (str
   (make-id "def" form ns name idx)
   "-" 
   (clojure.string/join "-" (extract-defmethod-matches form))))

(defmethod make-id :default
  [type form ns name idx]
  (str ns "/" name "-" idx))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(declare install-capture! uninstall-capture! uninstall-captures-in-def!)

(defonce capture-records (atom {}))

(defn reset-capture-records!
  []
  (reset! capture-records {}))

(defn capture-records-for
  [ns-name name]
  (filter (fn [r] (->> r
                    ((juxt :ns :name))
                    (map str)
                    (= [(str ns-name) (str name)])))
          (vals @capture-records)))

(defn add-capture-record!
  [form source {:keys [name ns ast-idx pos], :as spec} existing-var]
  (let [type (type-of-def form)
        idx (or ast-idx (pos->ast-idx pos))
        indexed-node (nth (indexed-expr-list form) idx {})
        id (make-id type form ns name idx)
        id-spec (merge spec {:id id,
                             :source source,
                             :pos (:pos indexed-node),
                             :ast-idx idx,
                             :type type,
                             :defmethod-matches (map pr-str (extract-defmethod-matches form))})
        meta (meta existing-var)
        id-spec (if meta
                  (assoc id-spec :loc (select-keys meta [:column :end-column :line :end-line]))
                  id-spec)]
    (swap! capture-records assoc id id-spec)
    id-spec))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
 (agent-error storage)
 (clojure.repl/pst (agent-error storage))
 (restart-agent storage {})
 (add-watch storage :capture-store (fn [k r o n] (println "store" n)))
 (remove-watch :capture-store)
 @storage
 )

(defonce storage (agent {}))

(defn captures
  "maps ids (def name + source loc) to vecs of capture entries. A capture entry
  is a map with the keys :value, :trace."
  []
  @storage)

(defn await-captures
  []
  (await storage)
  @storage)

(defn reset-storage!
  []
  (send storage empty))

(comment
 (-> @storage (get "user/hello-11") first :trace)

 )

(def ^{:dynamic true} *max-capture-count* 30)

(defn conj-limit
  [coll val]
  (conj
   (if (>= (count coll) *max-capture-count*)
     (butlast coll) coll)
   val))

(defn get-trace
  []
  (->> (.getStackTrace (Thread/currentThread))
    (drop 2)
    ; (map clojure.repl/stack-element-str)
    ; (clojure.string/join "\n")
    ))

(defn stringify-trace
  [trace]
  (->> trace
    (map clojure.repl/stack-element-str)
    (s/join "\n")))

(defmacro capture
  [loc form]
  `(let [val# ~form]
     (send storage update-in [~loc] conj-limit
           {:value val#
            :trace (st/parse-trace-elems (get-trace))
            :time (System/currentTimeMillis)})
     val#))

(defn inspect
  [id & [n]]
  (let [values (get @storage id)]
    (if n (-> values (nth n) :value) (map :value values))))

(defn captures->json
  [& {:keys [nss only-last], :or {nss :all, only-last false}}]
  (let [records (if (= nss :all) (vals @capture-records)
                  (filter #(some #{(-> % :ns str)} nss) (vals @capture-records)))
        massage-data (fn [{id :id :as r}]
                       (let [stored (get @storage id [])]
                         (-> r
                           (assoc :values (map (comp pr-str :value) stored)
                                  :traces (map :trace stored)
                                  :times (map :time stored))
                           (dissoc :source)
                           (update-in [:ns] str))))]
    (->> records
      (map massage-data)
      json/write-str)))

(comment
 (captures->json :nss ["user"])
 (captures->json :nss :all)
  @capture-records
  @storage  
 )

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn eval-form  
  [form ns & [existing add-meta]]
  (let [name (name-of-def form)
        sym (symbol (str ns) (str name))
        m (merge (if existing (meta existing) {})
                 (or add-meta {}))]
    (binding [*ns* ns] (eval form))
    (let [new-def (find-var sym)]
      (alter-meta! new-def merge m))))

(defn find-existing-def
  [spec]
  (find-var (symbol (str (:ns spec)) (str (:name spec)))))

(comment
 (reset! re-install-log [])
 re-install-log
 )

(defonce re-install-log (atom []))

(defn re-install-on-redef
  [spec src]
  (if-let [v (find-existing-def spec)]
    (add-watch
     v :cloxp-capture-reinstall
     (fn [k r o n]
       (if (and r k) (try (remove-watch r k) (catch Exception e nil)))
       (let [capt-data (::capturing (meta r))
             records (apply capture-records-for
                            ((juxt (comp :ns meta) (comp :name meta)) r))]
         (if (not-empty records)
           (future
             (Thread/sleep 100)
             (let [m (meta r) ns (:ns m) name (:name m)
                   src (or
                        (:source m)
                        (clojure.repl/source-fn (symbol (str ns) (str name))))
                   h (if src (hash src))]
               (if (and src capt-data
                        (not-empty records)
                        (= (:hash capt-data) h))
                 (install-capture! src
                                   :ns ns
                                   :name name
                                   :ast-idx (:ast-idx (first records)))
                 (if (not-empty records)
                   (uninstall-captures-in-def! (-> records first :id))))
               ))))))))

(defn tfm-for-capture
  [form ids-and-idxs]
  (tfm/insert-captures-into-expr form ids-and-idxs))

(defn install-capture!
  [source & {ns :ns, name :name, :as spec}]
  (with-source source
    (let [form (src-rdr/read-with-source-logger source)
          existing (find-var (symbol (str (ns-name ns)) (str name)))
          spec-with-id (add-capture-record! form source spec existing)
          records-for-form (capture-records-for ns name)
          ids-and-idxs (map (fn [{:keys [id ast-idx]}] [id ast-idx]) records-for-form)
          traced-form (tfm-for-capture form ids-and-idxs)]
      (if (and existing (not-empty (-> existing .getWatches)))
        (remove-watch  existing :cloxp-capture-reinstall))
      (eval-form traced-form ns existing {::capturing {:hash (hash source)}, :source source})
      (re-install-on-redef spec-with-id source)
      spec-with-id)))

(defn empty-capture!
  [id]
  (send storage dissoc id))

(defn uninstall-capture!
  [id]
  (empty-capture! id)
  (if-let [spec (get @capture-records id)]
    (let [{id :id} spec]
      ; 1.forget record and captures
      (swap! capture-records dissoc id)
      ; 2. uninstall re-def watchers
      (let [existing-var (find-existing-def spec)]
        (if (and existing-var (-> existing-var .getWatches not-empty))
          (remove-watch existing-var :cloxp-capture-reinstall)))
      ; 3. if the var still includes captures then re-eval it to get rid of them
      (let [existing (find-existing-def spec)
            m (meta existing)
            old-hash (hash (:source spec))
            new-hash (-> m ::capturing :hash)]
        (if (and new-hash (= new-hash old-hash))
          (eval-form (read-string (:source spec)) (:ns spec) existing))))))

(defn uninstall-captures-in-def!
  [id]
  (if-let [spec (get @capture-records id)]
    (let [ns-and-name (select-keys spec [:name :ns])
          recs-in-same-def (filter
                            #(= ns-and-name (select-keys (val %) [:name :ns]))
                            @capture-records)]
      (doseq [[id _] recs-in-same-def]
        (uninstall-capture! id)))))

(defn reset-captures!
  []
  (try (->> @capture-records keys (map uninstall-capture!) doall)
    (catch Exception e nil))
  (reset-storage!)
  (reset-capture-records!))


; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment

 (install-capture! '(defn foo [] (Math/round (* 100 (rand)))) :ns *ns* :ast-idx 4 :name "foo")
 (defn foo [] (Math/round (* 100 (rand))))
 (foo)
 (await-captures)
 (get @capture-records "foo-4")

 (rksm.cloxp-trace.capturing/captures->json)
 (str "foooo" (clojure.repl/pst (try (rksm.cloxp-trace/captures->json) (catch Exception e e)) 20))
 )
