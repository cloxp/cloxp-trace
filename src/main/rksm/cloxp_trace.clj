(ns rksm.cloxp-trace
  (:require [clojure.zip :as z])
  (:require [clojure.data.json :as json])
  (:require [rksm.cloxp-trace.transform :as tfm])
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
  [form {name :name, idx :ast-idx, ns :ns, :as spec}]
  (let [id (str ns "/" name "-" idx)
        id-spec (assoc spec :id id :form form)]
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

(defmacro capture
  [loc form]
  `(let [val# ~form]
    (send storage update-in [~loc] conj val#)
     val#)
  )

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
    (alter-meta! new-def merge m)))

(defn find-existing-def
  [spec]
  (find-var (symbol (str (:ns spec)) (str (:name spec)))))

(comment
 (remove-watch #'rksm.cloxp-trace-test/def-for-capture :cloxp-capture-reinstall)
 )

(defn re-install-on-redef
  [spec]
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
             src (try (repl/source-fn sym) (catch Exception e (do e nil)))
             form (if src (read-string src))
             h (if form (hash form))]
         [r k]
         (if (and r k) (try (remove-watch r k) (catch Exception e nil)))
         (if (and src capt-data
                  (not-empty records)
                  (= (:hash capt-data) h))
           (future (Thread/sleep 100) (do (install-capture! form (first records)) "OK"))
           (do 
             (if (not-empty records)
               (uninstall-capture! (-> records first :id)))))
         )
       ))))

(defn tfm-for-capture
  [form ids-and-idxs]
  (tfm/insert-captures-into-expr form ids-and-idxs))

(defn install-capture!
  [form & {ns :ns, name :name, :as spec}]
  (let [spec-with-id (add-capture-record! form spec)
        records-for-form (capture-records-for ns name)
        ids-and-idxs (map (fn [{:keys [id ast-idx]}] [id ast-idx]) records-for-form)
        traced-form (tfm-for-capture form ids-and-idxs)
        existing (find-existing-def spec-with-id)
        bound (find-var (symbol (str ns) (str name)))]
    ; (println (symbol (str ns) (str name)))
    (if (and bound (not-empty (-> bound .getWatches)))
       (remove-watch  bound :cloxp-capture-reinstall))
    (eval-form traced-form ns existing {::capturing {:hash (hash form)}})
    (re-install-on-redef spec-with-id)
    spec-with-id
    ))

(defn empty-capture!
  [id]
  (send storage update-in [id] empty))

(defn uninstall-capture!
  [id]
  (if-let [spec (get @capture-records id)]
    (if (:form spec)
      (do
        (eval-form (:form spec) (:ns spec) (find-existing-def spec))
        (swap! capture-records dissoc id))
      (throw (Exception. (str "cannot uninstall " id ", no form!"))))
    (throw (Exception. (str "cannot uninstall " id ", no capture record!")))))

(defn reset-captures!
  []
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