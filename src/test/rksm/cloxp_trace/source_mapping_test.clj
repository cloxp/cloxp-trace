(ns rksm.cloxp-trace.source-mapping-test
  (:require [midje.sweet :refer :all])
  (:require [rksm.cloxp-trace.source-mapping :refer :all]))

(def code-1 "(defn foo [x]
  (+ 23 x)
  [{:x 3, :y 5}])")

(comment
 (debug-positions code-1)
 (debug-positions "(#'foo %)")
 
 )

(facts "pos to index mapping"
  (facts "ab|cde" (pos->idx {:column 2, :line 1} :source "abcde") => 2)
  (facts "a\nbbb|cde" (pos->idx {:column 3, :line 2} :source "a\nbbbcde") => 5))

(facts "index to pos mapping"
  (facts "ab|cde" (idx->pos 2 :source "abcde") => {:column 2, :line 1})
  (facts "a\nbbb|cde" (idx->pos 5 :source "a\nbbbcde") => {:column 3, :line 2}))

(facts "attach positions to non-meta forms"
  (facts "numbers" (debug-positions "(aa 23)") => '([0 (aa 23) {:end-column 8, :end-line 1, :column 1, :line 1}]
                                                    [1 aa {:end-column 4, :end-line 1, :column 2, :line 1}]
                                                    [2 23 nil])))

(facts "find ast index for source pos"
  
  (facts "|(+ 23 x)"    (pos->ast-idx code-1 {:line 2, :column 3}) => 5)
  (facts "|  (+ 23 x)"  (pos->ast-idx code-1 {:line 2, :column 3}) => 5)     ; (ast, 16).idx)
  (facts "  (+| 23 x)"  (pos->ast-idx code-1 {:line 2, :column 5}) => 7)     ; (ast, 20).idx)
  (facts "  (+ |23 x)"  (pos->ast-idx code-1 {:line 2, :column 6}) => 7)     ; (ast, 20).idx)
  (facts "|(defn ...)|" (pos->ast-idx code-1 {:line 1, :column 0}) => 0)     ; (ast, 0, testCode.length).idx)
  (facts "|(+ 23 x)|"   (pos->ast-idx code-1 {:line 3, :column 3}) => 9)     ; (ast, 29).idx)

  )

(comment
 (facts "find ast index for source pos"
   
   (facts "|(+ 23 x)" (pos->ast-idx code-1 {:line 2, :column 3}) => 5)
   ;   (facts "|  (+ 23 x)|" (pos->ast-idx code-1 {:line 2, :column 0}) => 5)     ; (ast, 16, 26).idx)
   (facts "|  (+ 23 x)"  (pos->ast-idx code-1 {:line 2, :column 3}) => 5)     ; (ast, 16).idx)
   (facts "  (+| 23 x)"  (pos->ast-idx code-1 {:line 2, :column 5}) => 7)     ; (ast, 20).idx)
   ;   (facts "  (+| 23 x|)" (pos->ast-idx code-1 {:line 2, :column 3}) => 7)     ; (ast, 20, 25).idx)
   (facts "|(defn ...)|" (pos->ast-idx code-1 {:line 1, :column 0}) => 0)     ; (ast, 0, testCode.length).idx)
   (facts "|(+ 23 x)|"   (pos->ast-idx code-1 {:line 3, :column 3}) => 9)     ; (ast, 29).idx)
   
   ))

(comment
 (require '[midje.repl])
 (midje.config/merge-permanently! {:colorize false})
 (midje.emission.colorize/init!)

 (midje.repl/load-facts :all)
 (midje.repl/check-facts)
 
 )