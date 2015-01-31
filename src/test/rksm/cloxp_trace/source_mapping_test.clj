(ns rksm.cloxp-trace.source-mapping-test
  (:require [clojure.test :refer :all])
  (:require [rksm.cloxp-trace.source-mapping :refer :all]))

(defmacro check
  ([descr expr _ expected] `(testing ~descr (check ~expr _ ~expected)))
  ([expr _ expected] `(is (= ~expr ~expected))))

(def code-1 "(defn foo [x]
  (+ 23 x)
  [{:x 3, :y 5}])")

(deftest pos-to-index-mapping
  (check "|abcde" (pos->idx {:column 1, :line 1} "abcde") => 0)
  (check "ab|cde" (pos->idx {:column 3, :line 1} "abcde") => 2)
  (check "a\nbbb|cde" (pos->idx {:column 4, :line 2} "a\nbbbcde") => 5)
  (check "multi line" (pos->idx {:column 1, :line 2} code-1) => 14))

(deftest index-to-pos-mapping
  (check "ab|cde" (idx->pos 2 "abcde") => {:column 3, :line 1})
  (check "a\nbbb|cde" (idx->pos 5 "a\nbbbcde") => {:column 4, :line 2}))

(deftest with-current-source
  (check (with-source code-1 (pos->idx {:column 2, :line 2})) => 15)
  (check (with-source code-1 (pos->idx {:column 1, :line 2})) => 14)
  (check (with-source code-1 (idx->pos 15)) => {:column 2, :line 2})
  (check (with-source code-1 (idx->pos 14)) => {:column 1, :line 2}))

(deftest position-for-tracing
  (check (pos->node-idx-to-trace {:column 3, :line 2} code-1) => 5))

(deftest find-ast-index-for-source-pos

  (check "|(+ 23 x)"    (pos->ast-idx {:line 2, :column 3} code-1) => 5)
  (check "|  (+ 23 x)"  (pos->ast-idx {:line 2, :column 1} code-1) => 0)     ; ???
  (check "  (+ 23 |x)"  (pos->ast-idx {:line 2, :column 9} code-1) => 8)
  (check "  (+| 23 x)"  (pos->ast-idx {:line 2, :column 6} code-1) => 6)     ; currently "+"
  (check "  (+ |23 x)"  (pos->ast-idx {:line 2, :column 7} code-1) => 5)     ; currently outer expr
  (check "|(defn ...)|" (pos->ast-idx {:line 1, :column 1} code-1) => 0)
  (check "|(+ 23 x)|"   (pos->ast-idx {:line 3, :column 4} code-1) => 10))

(deftest find-ast-index-for-source-idx
  (check (idx->ast-idx 16 code-1) => 5))


(comment

 (run-tests 'rksm.cloxp-trace.source-mapping-test)

 (require '[clojure.zip :as z])

 (some->> (source->indexed-tree-zipper code-1)
   (pos->zpprs {:column 3, :line 2})
   (map z/node))

 (pos->idx {:column 0, :line 2} :source code-1)

 (some->> (source->indexed-tree-zipper code-1)
   (pos->zpprs {:column 0, :line 2})
   (map z/node))

 (debug-positions code-1)
 code-1
 (-> (debug-positions "(#'foo %)")
   (nth 2)
   str)

  )