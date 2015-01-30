(ns rksm.cloxp-trace.source-mapping
  (:require [clojure.zip :as z])
  (:require [rksm.cloxp-trace.transform :as tfm])
  (:require [clojure.tools.reader :as tr])
  (:require [clojure.tools.reader.reader-types :as trt])
  (:require [clojure.string :as s]))

(require '[clojure.tools.trace :as t])
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; pos mapping
(defn pos->idx
  [{:keys [line column] :as pos} & {:keys [source lines]}]
  (if (and (not lines) (not source))
    (throw (Exception. "pos->idx needs either source or lines!")))
  pos
  (let [lines (take line (or lines (s/split-lines source)))
        line (nth lines (dec line) "")
        idx (apply + column (-> lines count dec) (map count (drop-last lines)))]
    idx))

(defn idx->pos
  [idx & {:keys [lines source]}]
  (if (and (not lines) (not source))
    (throw (Exception. "idx->pos needs either source or lines!")))
  (let [source (or source (s/join "\n" lines))
        subs (.substring source 0 idx)
        lines (s/split-lines subs)
        line (last lines)]
    {:column (count line) :line (count lines)}))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; reading

(defn read-with-source-logger
  [source]
  (let [rdr (trt/source-logging-push-back-reader source)]
    (tr/read rdr)))

(defn src->form-zipper
  [source]
  (tfm/tree-zipper
   (read-with-source-logger source)))

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
  (let [source (or (some-> form meta :source) "")]
   (loop [zppr (tfm/tree-zipper form)
          seen-zpprs [], ctxs []]
     (if (z/end? zppr)
       ctxs
       (let [parent (.indexOf seen-zpprs (z/up zppr))
             node (z/node zppr)
             pos (idx-node->source-pos node)
             lines (s/split-lines source)
             ctxs (conj ctxs {:idx (count seen-zpprs)
                              :parent (if (= -1 parent) nil parent)
                              :form node
                              :source (:source (meta node))
                              :pos pos
                              :pos-idx (if pos (pos->idx pos :lines lines))})
             seen-zpprs (conj seen-zpprs zppr)]
         (recur (z/next zppr) seen-zpprs ctxs))))))

(comment
 (indexed-expr-list (read-with-source-logger "(aaa (bbb))"))
 )

(defn indexed-tree-zipper [s]
  (let [g (group-by :parent s)] 
    (z/zipper
     (comp g :idx)
     #(g (:idx %))
     nil
     (first (g nil)))))

(defn source->indexed-tree-zipper
  [source]
  (-> source
    read-with-source-logger
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

(defn- pos->zpprs
  [pos zppr]
  (if-not (z/branch? zppr)
    (list zppr)
    (let [down (z/down zppr)
          downs (take-while (complement nil?) (iterate z/right down))
          _ (->> downs (map (comp :form z/node)))
          included (some->> downs
                     reverse
                     (filter (fn [z] (includes-pos? pos (-> z z/node :form idx-node->source-pos))))
                     first
                     (pos->zpprs pos))]
      (cons zppr (or included (list))))))

(defn pos->ast-idx
  [src pos]
  (some->> (source->indexed-tree-zipper src)
    (pos->zpprs pos)
    last z/node :idx))

(defn debug-positions
  [src]
  (->> (source->indexed-tree-zipper src)
    (iterate z/next)
    (take-while (complement z/end?))
    (map z/node)
    (map (juxt :idx :form (comp #(select-keys % [:column :line]) idx-node->source-pos :form) :pos-idx))
    ))

(comment
 (->> (pos->zpprs {:column 7 :line 1} zppr)
   (map (comp :idx z/node)))

 (def zppr (source->indexed-tree-zipper "(aaa ({:x (a), :y 23} ccc))"))

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