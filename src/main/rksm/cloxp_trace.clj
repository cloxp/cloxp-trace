(ns rksm.cloxp-trace
  (:require [clojure.zip :as z])
  (:require [clojure.data.json :as json])
  (:require [rksm.cloxp-trace.transform :as tfm])
  (:require [rksm.cloxp-trace.source-mapping :refer (with-source pos->ast-idx read-with-source-logger)])
  (:require [clojure.repl :as repl]))

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

(def capture-records (atom {}))

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
  [form {:keys [name ns ast-idx pos], :as spec} existing-var]
  (let [type (type-of-def form)
        idx (or ast-idx (pos->ast-idx pos))
        indexed-node (nth (rksm.cloxp-trace.source-mapping/indexed-expr-list form) idx {})
        id (make-id type form ns name idx)
        id-spec (merge spec {:id id,
                             :form form,
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

(def storage (agent {}))

(defn captures
  []
  @storage)

(defn await-captures
  []
  (await storage)
  @storage)

(defn reset-storage!
  []
  (send storage empty))

(def ^{:dynamic true} *max-capture-count* 30)

(defn conj-limit
  [coll val]
  (conj
   (if (>= (count coll) *max-capture-count*)
     (butlast coll) coll)
   val))

(defmacro capture
  [loc form]
  `(let [val# ~form]
     (send storage update-in [~loc] conj-limit val#)
     val#))

(defn captures->json
  [& {:keys [nss], :or {nss :all}}]
  (let [records (if (= nss :all) (vals @capture-records)
                  (filter #(some #{(-> % :ns str)} nss) (vals @capture-records)))
        massage-data (fn [r]
                       (-> r
                         (assoc :last-val (pr-str (first (get @storage (:id r) []))))
                         (dissoc :form)
                         (update-in [:ns] str)))]
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

; (binding [*ns* "user"] (println *ns*))

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
                   form (if src (read-string src))
                   h (if form (hash form))]
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
    (let [form (read-with-source-logger)
          existing (find-var (symbol (str (ns-name ns)) (str name)))
          spec-with-id (add-capture-record! form spec existing)
          records-for-form (capture-records-for ns name)
          ids-and-idxs (map (fn [{:keys [id ast-idx]}] [id ast-idx]) records-for-form)
          traced-form (tfm-for-capture form ids-and-idxs)]
      (if (and existing (not-empty (-> existing .getWatches)))
        (remove-watch  existing :cloxp-capture-reinstall))
      (eval-form traced-form ns existing {::capturing {:hash (hash form)}, :source source})
      (re-install-on-redef spec-with-id source)
      spec-with-id)))

(defn empty-capture!
  [id]
  (send storage dissoc id))

(defn uninstall-capture!
  [id]
  (if-let [spec (get @capture-records id)]
    (let [{id :id} spec]
      ; 1.forget record and captures
      (swap! capture-records dissoc id)
      (empty-capture! id)
      ; 2. uninstall re-def watchers
      (let [existing-var (find-existing-def spec)]
        (if (and existing-var (-> existing-var .getWatches not-empty))
          (remove-watch existing-var :cloxp-capture-reinstall)))
      ; 3. if the var still includes captures then re-eval it to get rid of them
      (let [existing (find-existing-def spec)
            m (meta existing)
            old-hash (hash (:form spec))
            new-hash (-> m ::capturing :hash)]
        (if (and new-hash (= new-hash old-hash))
          (eval-form (:form spec) (:ns spec) existing))))))

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

 (rksm.cloxp-trace/captures->json)
 (str "foooo" (clojure.repl/pst (try (rksm.cloxp-trace/captures->json) (catch Exception e e)) 20))
 )