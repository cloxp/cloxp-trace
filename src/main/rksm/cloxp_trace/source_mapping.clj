(ns rksm.cloxp-trace.source-mapping
  (:require [clojure.zip :as z]
            [rksm.cloxp-trace.transform :as tfm]
            [rksm.cloxp-source-reader.core :as src-rdr]
            [clojure.string :as s]))

(def ^{:dynamic true} *current-code*)

(defmacro with-source
  [source & body]
  `(do
     (assert (string? ~source))
     (binding [*current-code* {:source ~source
                               :lines (s/split-lines ~source)}]
       ~@body)))

(defn get-current
  [key]
  (assert *current-code* "*current-code* not defined!")
  (assert (contains? *current-code* key) (str key " not in *current-code*"))
  (get *current-code* key))

(defn current-source [] (get-current :source))
(defn current-lines [] (get-current :lines))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; pos mapping

(defn pos->idx
  ([{:keys [line column] :as pos}]
   (if (nil? *current-code*)
     (throw (Exception. "pos->idx needs *current-code* set!")))
   (let [lines (take line (current-lines))
         line (nth lines (dec line) "")
         idx (apply + (dec column) (-> lines count dec) (map count (drop-last lines)))]
     idx))
  ([pos source] (with-source source (pos->idx pos))))

(defn idx->pos
  ([idx]
   (if (nil? *current-code*)
     (throw (Exception. "idx->pos needs either source or lines!")))
   (let [subs (.substring (current-source) 0 idx)
         at-nl (.endsWith subs "\n")
         lines (s/split-lines subs)
         lines (if at-nl (conj lines "") lines)
         line (last lines)]
     {:column (inc (count line)) :line (count lines)}))
  ([idx source] (with-source source (idx->pos idx))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; data structures for tree traversal / indexing

(defn- idx-node->source-pos
  [node]
  (if-let [meta (meta node)]
    (let [pos (select-keys meta [:line :column :end-line :end-column])]
      (if (empty? pos) nil pos))))

(defn indexed-expr-list
  "(aaa (bbb))
  =>
  [{:idx 0, :parent nil, :form (aaa (bbb))}
  {:idx 1, :parent 0, :form aaa}
  {:idx 2, :parent 0, :form (bbb)}
  {:idx 3, :parent 2, :form bbb}]"
  [form]
  (with-source (or (some-> form meta :source) "")
    (loop [zppr (tfm/tree-zipper form)
           seen-zpprs [], ctxs []]
      (if (z/end? zppr)
        ctxs
        (let [parent (.indexOf seen-zpprs (z/up zppr))
              node (z/node zppr)
              pos (idx-node->source-pos node)
              ctxs (conj ctxs {:idx (count seen-zpprs)
                               :parent (if (= -1 parent) nil parent)
                               :form node
                               :source (:source (meta node))
                               :pos pos
                               :pos-idx (if pos (pos->idx pos))})
              seen-zpprs (conj seen-zpprs zppr)]
          (recur (z/next zppr) seen-zpprs ctxs))))))

(comment
 (indexed-expr-list (src-rdr/read-with-source-logger "(aaa (bbb))"))
 )

(defn indexed-tree-zipper
  [idxd-exprs]
  (let [g (group-by :parent idxd-exprs)]
    (z/zipper
     (comp g :idx)
     #(g (:idx %))
     nil
     (first (g nil)))))

(defn indexed-tree-zipper-from-source
  []
  (-> *current-code*
    :source
    src-rdr/read-with-source-logger
    indexed-expr-list
    indexed-tree-zipper))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; source -> ast mapping

(defn includes-pos?
  [{pos-column :column, pos-line :line, :as pos}
   {:keys [line column end-line end-column], :as form}]
  [form pos (and (every? boolean [line column end-line end-column])
             (<= line pos-line) (<= column pos-column)
             (<= pos-line end-line) (<= pos-column end-column))]
  (and (every? boolean [line column end-line end-column])
       (<= line pos-line) (<= column pos-column)
       (<= pos-line end-line) (<= pos-column end-column)))

(defn zipper-includes-pos?
  [pos z]
  (includes-pos?
   pos (-> z z/node :form idx-node->source-pos)))

(defn pos->zpprs
  [pos zppr]
  (if-not (z/branch? zppr)
    (list zppr)
    (->> zppr
      (iterate z/next)
      (take-while (complement z/end?))
      (filter (partial zipper-includes-pos? pos)))))

(defn pos->ast-idx
  ([pos]
   (some->> (indexed-tree-zipper-from-source)
     (pos->zpprs pos)
     last z/node :idx))
  ([pos src] (with-source src (pos->ast-idx pos))))

(defn idx->ast-idx
  ([idx] (pos->ast-idx (idx->pos idx)))
  ([idx src] (with-source src (idx->ast-idx idx))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; tracing related

(defn pos->node-idx-to-trace
  ([pos]
   (some->> (indexed-tree-zipper-from-source)
     (pos->zpprs pos)
     last z/node :idx))
  ([pos src] (with-source src (pos->node-idx-to-trace pos))))


; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn debug-positions
  ([]
   (->> (indexed-tree-zipper-from-source)
     (iterate z/next)
     (take-while (complement z/end?))
     (map z/node)
     (map (juxt :idx :form (comp #(select-keys % [:column :line]) idx-node->source-pos :form) :pos-idx))
     ))
  ([src] (with-source src (debug-positions))))

(comment
 (->> (pos->zpprs {:column 7 :line 1} zppr)
   (map (comp :idx z/node)))

 (def zppr (with-source "(aaa ({:x (a), :y 23} ccc))" (indexed-tree-zipper-from-source)))

 (z/branch? zppr)


 (-> zppr z/down z/next z/down z/node :form meta)
 (-> zppr z/down z/right z/right)
 (-> zppr z/down z/next z/down z/node idx-node->source-pos)
 )

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; DEPRECATED
; source -> ast mapping


(defn containing-exprs
  [pos form-seq]
  (filter (partial includes-pos? pos) form-seq))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment

 )