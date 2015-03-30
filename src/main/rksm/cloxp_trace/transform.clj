(ns rksm.cloxp-trace.transform
  (:require [clojure.zip :as z]
            [clojure.pprint :refer :all])
  (:import (clojure.lang IPersistentVector IPersistentMap IPersistentList ISeq)))

; Thx @ Alex Miller! http://www.ibm.com/developerworks/library/j-treevisit/

(defmulti tree-branch? class)
(defmethod tree-branch? :default [_] false)
(defmethod tree-branch? IPersistentVector [v] (not-empty v))
(defmethod tree-branch? IPersistentMap [m] (not-empty m))
(defmethod tree-branch? IPersistentList [l] true)
(defmethod tree-branch? ISeq [s] true)
(prefer-method tree-branch? IPersistentList ISeq)

(defmulti tree-children class)
(defmethod tree-children IPersistentVector [v] v)
(defmethod tree-children IPersistentMap [m] (->> m seq (apply concat)))
(defmethod tree-children IPersistentList [l] l)
(defmethod tree-children ISeq [s] s)
(prefer-method tree-children IPersistentList ISeq)

(defmulti tree-make-node (fn [node children] (class node)))
(defmethod tree-make-node IPersistentVector [v children]
  (vec children))
(defmethod tree-make-node IPersistentMap [m children]
  (apply hash-map children))
(defmethod tree-make-node IPersistentList [_ children]
  children)
(defmethod tree-make-node ISeq [node children]
  (apply list children))
(prefer-method tree-make-node IPersistentList ISeq)

(defn tree-zipper [node]
  (z/zipper tree-branch? tree-children tree-make-node node))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn print-tree
  "for debugging"
  [node]
  (let [all (take-while (complement z/end?) (iterate z/next (tree-zipper node)))]
    (binding [*print-right-margin* 20] 
     (pprint
      (->> all
        (map z/node) (zipmap (range))
        sort)))))

(defn tfm-visit
  [zippd i ids-and-idxs]
  (if-let [[id _] (first (filter (fn [[_ idx]] (= i idx)) ids-and-idxs))]
    (z/edit zippd (fn [n] `(rksm.cloxp-trace.capturing/capture ~id ~n)))
    zippd))

(defn insert-captures-into-expr
  "Takes a clojure expression and for each idx inserts a (capture _)
  expression, wrapping the original node. Idx is a pointer into the expression
  tree, in the order of iterative left-to-right traversal.
  Example, given '(+ 2 (- 3 4)):
  0: (+ 2 (- 3 4))
  1: +
  2: 2
  3: (- 3 4)
  4: -
  5: 3
  6: 4"
  [expr ids-and-idxs]
  (let [root (tree-zipper expr)
        all (-> (take-while (complement z/end?) (iterate z/next root)))
        last-with-counting-ctx {:i (count all), :z (last all)}
        visit-and-prev (fn [{:keys [i z]}] 
                         {:i (dec i),
                          :z (z/prev (tfm-visit z (dec i) ids-and-idxs))})
        it (iterate visit-and-prev last-with-counting-ctx)
        tfmed (take-while (comp not nil? :z) it)]
    (-> tfmed last :z z/node)))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
 (insert-captures-into-expr '(+ 2 (- 3 4)) [["test" 3]])
 (insert-captures-into-expr '(foo 2 {:x (+ 3 4)}) [["a" 3] ["b" 5]])
 (insert-captures-into-expr '((if x y) 23) [["a" 4]])
 (print-tree '({} {:x 3}))
 (print-tree '(defn install-capture!
  [form & {ns :ns, name :name, :as spec}]
  (let [spec-with-id (add-capture-record! form spec)
        records-for-form (capture-records-for ns name)
        ids-and-idxs (map (fn [{:keys [id ast-idx]}] [id ast-idx]) records-for-form)
        traced-form (tfm-for-capture form ids-and-idxs)
        existing (find-existing-def spec-with-id)]
    [ns name]
    (remove-watch (find-var (symbol (str ns) (str name))) :cloxp-capture-reinstall)
    (eval-form traced-form ns existing {::capturing {:hash (hash form)}})
    (re-install-on-redef spec-with-id)
    spec-with-id
    )))
 (seq {})
 )
