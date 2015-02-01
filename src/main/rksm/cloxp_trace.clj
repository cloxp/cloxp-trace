(ns rksm.cloxp-trace
  (:require [clojure.zip :as z])
  (:require [clojure.data.json :as json])
  (:require [rksm.cloxp-trace.transform :as tfm])
  (:require [rksm.cloxp-trace.source-mapping :refer (with-source pos->ast-idx read-with-source-logger)])
  (:require [clojure.repl :as repl]))


(declare install-capture! uninstall-capture!)

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
  pos
  (let [idx (or ast-idx (pos->ast-idx pos))
        indexed-node (nth (rksm.cloxp-trace.source-mapping/indexed-expr-list form) idx {})
        id (str ns "/" name "-" idx)
        id-spec (assoc spec :id id, :form form,
                       :pos (:pos indexed-node),
                       :ast-idx idx)
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
   (if (> (count coll) *max-capture-count*)
     (drop 1 coll) coll)
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
  (let [m (merge (if existing (meta existing) {})
                 (or add-meta {}))
        new-def (binding [*ns* ns] (eval form))]
    (alter-meta! new-def merge m)
    (meta new-def)))

(defn find-existing-def
  [spec]
  (find-var (symbol (str (:ns spec)) (str (:name spec)))))

(comment
 (remove-watch #'rksm.cloxp-trace-test/def-for-capture :cloxp-capture-reinstall)
 )

(defonce re-install-log (atom []))

(defn re-install-on-redef
  [spec src]
  (if-let [v (find-existing-def spec)]
    (add-watch
     v :cloxp-capture-reinstall
     (fn [k r o n]
       (let [m (meta r)
             capt-data (::capturing m)
             ns (:ns m)
             name (:name m)
             records (capture-records-for ns name)
             sym (symbol (str ns) (str name))
             form (if src (read-string src))
             h (if form (hash form))]
         [r k]
         (if (and r k) (try (remove-watch r k) (catch Exception e nil)))
         (if (and src capt-data
                  (not-empty records)
                  (= (:hash capt-data) h))
           (future (Thread/sleep 100)
                   (do
                     (install-capture! src :ns ns :name name :ast-idx (:ast-idx (first records)))
                     "OK"))
           (do 
             (if (not-empty records)
               (uninstall-capture! (-> records first :id)))))
         )))))

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
      (eval-form traced-form ns existing {::capturing {:hash (hash form)}})
      (re-install-on-redef spec-with-id source)
      spec-with-id)))

(defn empty-capture!
  [id]
  (send storage update-in [id] empty))

(defn uninstall-capture!
  [id]
  (if-let [spec (get @capture-records id)]
    (do 
      (let [existing-var (find-existing-def spec)]
        (if (and existing-var (-> existing-var .getWatches not-empty))
          (remove-watch existing-var :cloxp-capture-reinstall)))
      (if (:form spec)
        (do
          (eval-form (:form spec) (:ns spec) (find-existing-def spec))
          (swap! capture-records dissoc id))))))

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