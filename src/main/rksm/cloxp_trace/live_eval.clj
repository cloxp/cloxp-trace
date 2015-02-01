(ns rksm.cloxp-trace.live-eval
;   (:require [clo])
  (:require [clojure.tools.reader :as tr])
  (:require [clojure.tools.reader.reader-types :as trt])
  (:require [clojure.string :as s]))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; FIXME duplicated form rksm.system-navigator.ns.internals, refactor!

(defn purge-string!
  [rdr]
  (let [buf (-> rdr .rdr .source_log_frames var-get :buffer)
        str (.toString buf)]
    (.delete buf 0 (count str))
    str))

(defn read-objs
  [rdr-or-src]
  ; FIXME this is hacked...
  (let [rdr (trt/indexing-push-back-reader (trt/source-logging-push-back-reader rdr-or-src))]
    (loop [result []]
      (let [start-line (trt/get-line-number rdr)
            start-column (trt/get-column-number rdr)]
        (if-let [o (tr/read rdr false nil)]
          (let [raw-str (purge-string! rdr)
                lines (s/split-lines raw-str)
                no-ws-lines (take-while #(re-find #"^\s*(;.*)?$" %) lines)
                src-lines (drop (count no-ws-lines) lines)
                first-line-ws-match (re-matches #"^(\s*)(.*)" (first src-lines))
                src-lines (assoc (vec src-lines) 0 (nth first-line-ws-match 2))
                src (s/join "\n" src-lines)
                line (+ (count no-ws-lines) start-line)
                column (+ start-column (count (second first-line-ws-match)))]
            (when (= \newline (trt/peek-char rdr))
              (trt/read-char rdr)
              (purge-string! rdr))
            (recur (conj result {:form o
                                 :source src
                                 :line line
                                 :column column})))
          result)))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defmulti process-result class)

(defmethod process-result clojure.lang.Var
  [var]
  (str var "=> " (deref var)))

(defmethod process-result java.lang.Exception
  [e]
  (pr-str e)
;   (clojure.repl/pst e 40)
;   "error"
  )

(defmethod process-result :default
  [x]
  (pr-str x))

(defn eval-code
  [{:keys [form line column]}]
  (let [
        ; meta (or (meta form) {})
        ; pos (select-keys meta [:line :column :end-line :end-column])
        value (process-result (try (eval form) (catch Exception e e)))]
    {:pos {:line line :column column}, :value value}))

(defn live-eval-code
  [code & {:as opts}]
  (->> (read-objs code)
    (map eval-code)
    doall))