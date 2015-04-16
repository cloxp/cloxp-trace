(ns rksm.cloxp-trace.tracing
  (:refer-clojure :exclude [replace])
  (:require [clojure.tools.trace :as trace]
            [clojure.string :refer [trim replace]]
            [clojure.zip :as z]
            [medley.core :refer [map-vals]]))

(defn trace-matching-ns
  [matcher]
  (doseq [n (all-ns)]
    (if (re-find matcher (str n))
      (trace/trace-ns n))))

(defn untrace-matching-ns
  [matcher]
  (doseq [n (all-ns)]
    (if (re-find matcher (str n))
      (trace/untrace-ns n))))

(defonce ^:private orig-tracer (deref #'trace/tracer))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; this tracer will replace for the duration of a call
; 'clojure.tools.trace/tracer. It does not modify the actual trace logic but
; will parse the output handed to it by clojure.tools.trace to get
; - function names, args, return values, exec times.
; Since data is handed to the tracer in printed(!) form, this approach doesn't
; give us the actual values returned.

(def ^{:private true :dynamic true} *tracer-off* false)

(def ^{:private true :dynamic true} *trace-results* nil)

(def ^{:private true :dynamic true} *intermediate-trace-results* nil)

(def ^:private re? (partial instance? java.util.regex.Pattern))

(defn- parse-call-start-val
  "something like
  | | | | | (foo/bar #<print-arg>"
  [val]
  (let [[_ depth name rest] (-> val
                              (replace #"\n" " ")
                              ((partial re-find #"^([\|\s]*)\(([^\s]+)\s+(.*)\)$")))]
    (if (nil? name) val)
    {:depth (if depth (count (clojure.string/split depth #" ")) 0)
     :printed-args rest
     :name name}))

(defn- parse-call-result-val
  "something like
  | | | | | => true"
  [val]
  (let [[_ printed-result] (re-find #"^[\|\s]*=>\s*(.*)$" val)]
    {:result printed-result}))

(defn- prepare-trace
  [trace-symbols]
  (doseq [s trace-symbols]
    (cond
      (re? s) (trace-matching-ns s)
      (symbol? s) (if (boolean (namespace s))
                    (trace/trace-var* (find-var s))
                    (trace/trace-ns s))
      :default nil)))

(defn- cleanup-trace
  [trace-symbols]
  (doseq [n (all-ns)] (trace/untrace-ns n)))

(defn cloxp-tracer
  [id value]
  (if (and (not *tracer-off*) *trace-results* *intermediate-trace-results*)
    (binding [*tracer-off* true]
      (let [t (.getTime (java.util.Date.))
            temp-data *intermediate-trace-results*
            frames *trace-results*]
        ; tracer gets called twice, once when traced fun is entered and when it
        ; returns
        (if-let [{:keys [start-time depth-fn-args parent-frame]} (get @temp-data id)]
          (do ; fun returns
            (swap! frames assoc id (merge
                                    (parse-call-start-val depth-fn-args)
                                    (parse-call-result-val value)
                                    {:call-id id
                                     :time (- t start-time)
                                     :parent-frame parent-frame}))
            (swap! temp-data dissoc id))
          (do ; fun entered
            (swap! temp-data assoc id {:start-time t
                                       :depth-fn-args value
                                       :parent-frame (some-> @temp-data
                                                       keys sort last)})))))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; processing trace results

(defn- timings
  [frames]
  (let
    [by-name (group-by :name (vals frames))
     time-per-fn (map-vals #(->> % (map :time) (apply +)) by-name)
     call-count-per-fn (map-vals count by-name)
     ;  normalized-call-time-per-fn (merge-with (comp float /)
     ;                                          time-per-fn call-count-per-fn)
     ]
    (->> (merge-with (fn [t c] {:time t, :count c, :normalized-time (float (/ t c))})
                     time-per-fn call-count-per-fn))))

(defn- indent
  [depth s & [indent-str]]
  (str (->> (range (or depth 0))
         (map (constantly (or indent-str " ")))
         (apply str))
       s))

(defn printed-in-call-order
  [frames-in-order]
  (let [sb (java.lang.StringBuffer.)]
    (doseq [{d :depth n :name r :result a :printed-args} frames-in-order]
      (.append sb (format
                   "%s [%s]\n%s => %s\n"
                   (indent d (clojure.string/trim (or n "no name!")) "| ")
                   a (indent d "" "| ") r)))
    (str sb)))

(defn- frames-in-order
  [frames]
  (let [roots (->> frames vals (filter (comp nil? :parent-frame)) (sort-by :call-id))
        root->zipper (fn [root]
                       (z/zipper
                        (fn [node] (> (count (:children node)) 0))
                        (fn [{:keys [children]}] (vals (select-keys frames children)))
                        (fn [node children] (assoc node :children children))
                        root))
        order-frames (fn [zppr]
                       (loop [loc zppr result []]
                         (if (z/end? loc) result
                           (recur (z/next loc) (conj result (-> loc z/node))))))]
    (->> roots (map root->zipper) (mapcat order-frames))))

(defn- process-trace-results
  [trace-results]
  (let [frames (->> trace-results vals)
        frames-with-children (->> frames
                               (map (fn [{:keys [call-id name] :as ea}]
                                      (assoc ea
                                             :name (or name "NO-NAME")
                                             :children
                                             (map :call-id
                                                  (filter
                                                   ;(comp (partial = call-id) :parent-frame)
                                                   (fn [{p :parent-frame}] (= p call-id))
                                                   frames)))))
                               (remove (comp (partial re-find #"^rksm.cloxp-trace.tracing/") :name))
                               (mapcat (juxt :call-id identity))
                               (apply hash-map))
        
        
        ; timings
        timings (timings frames-with-children)]
    (frames-in-order frames-with-children)))

(defn trace*
  [trace-symbols fn-to-trace]
  (if-not *trace-results*
    (binding [*trace-results* (atom {})
              *intermediate-trace-results* (atom {})]
      (with-redefs-fn {#'trace/tracer cloxp-tracer}
        (fn []
          (try
            (prepare-trace trace-symbols)
            (fn-to-trace)
            (finally (cleanup-trace trace-symbols)))))
      (process-trace-results @*trace-results*))))

(defmacro trace
  [trace-symbols & body]
  `(trace* ~trace-symbols (fn [] ~@body)))

(comment
 (def frames (trace
              [#"clojure.repl" #"clojure.core"]
              #_(clojure.string/split-lines "foo\nbar\nbaz")
              (clojure.repl/doc map)
              ))
 (-> frames printed-in-call-order print)
 )

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; a printing tracer, a simple alternative to the value based tracer above and
; slightly nicer than the original tracer

(defn- trace-start
  [name value time]
  (println (format "%s" (trim value))))

(defn- trace-end
  [name value time]
  ; (println (format "%s: %08d" (if name (str " " name) "") time))
  (println (format "%s (%05d ms)"
                   (str value)
                   #_(trim (replace (str value) #"([\s\|]+)(.*)" "$1"))
                   time)))

(defn cloxp-print-tracer
  [name value]
  (if (and (not *tracer-off*) *intermediate-trace-results*)
    (binding [*tracer-off* true]
      (let [t (.getTime (java.util.Date.))
            temp-data *intermediate-trace-results*]
        (if (contains? @temp-data name)
          (trace-end name value
                     (- t (get @temp-data name)))
          (do
            (swap! temp-data assoc name t)
            (trace-start name value t)))))))
