(ns rksm.cloxp-trace
  (:require [clojure.zip :as z]))


(def capture-records (atom {}))

(defn reset-capture-records!
  []
  (reset! capture-records {}))

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
                         (assoc :last-val (first (get @storage (:id r) [])))
                         (dissoc :form)
                         (update-in [:ns] str)))]
    (->> records
      (map massage-data)
      json/write-str
      )))

(comment

 (captures->json :nss ["user"])

 (captures->json :nss :all)
  @capture-records
  @storage
  
 )
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn linerized-tree-zip
  [form]
  (let [zppd (z/seq-zip form)]
    (take-while 
     (complement z/end?)
     (iterate z/next zppd))))

(defn tfm-for-capture
  [form {:keys [ast-idx id] :as opts}]
  (if-let [at-idx (nth (linerized-tree-zip form) ast-idx nil)]
    (z/root (z/edit at-idx (fn [n] `(capture ~id ~n))))))

(defn add-capture-record!
  [form {name :name, idx :ast-idx, :as spec}]
  (let [id (str name "-" idx)
        id-spec (assoc spec :id id :form form)]
    (swap! capture-records assoc id id-spec)
    id-spec))

(defn eval-form
  [form ns]
  (binding [*ns* ns]
    (eval form)))

(defn install-capture!
  [form & {ns :ns, :as spec}]
  (let [spec-with-id (add-capture-record! form spec)
        traced-form (tfm-for-capture form spec-with-id)]
    (eval-form traced-form ns)
    spec-with-id
    ))

(defn uninstall-capture!
  [id]
  (if-let [spec (get @capture-records id)]
    (if (:form spec)
      (do
        (eval-form (:form spec) (:ns spec))
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
 (foo)
 (await-captures)
 (get @capture-records "foo-4")
 (require '[clojure.data.json :as json])
 (rksm.cloxp-trace/captures->json)
 (str "foooo" (clojure.repl/pst (try (rksm.cloxp-trace/captures->json) (catch Exception e e)) 20))
 )