(ns rksm.cloxp-trace-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-trace :as t]))

(declare def-for-capture)

(defn capture-reset-fixture [test]
  (ns-unmap *ns* 'def-for-capture)
  (test) 
  (t/reset-captures!)
  (t/await-captures))

(use-fixtures :each capture-reset-fixture)

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(deftest capture-function
  (t/reset-captures!) (t/await-captures)
  (do
    (is (= 3 (t/capture "no-loc" (+ 1 2))))
    (is (= {"no-loc" [3]} (t/await-captures)))))


(deftest capture-transform
  
  (testing "single"
    (is (= '(/ (apply + a) (rksm.cloxp-trace/capture "test" (count (if (zero? (count a)) Double/POSITIVE_INFINITY a))))
           (t/tfm-for-capture '(/ (apply + a) (count (if (zero? (count a)) Double/POSITIVE_INFINITY a))) [["test" 6]]))))
  
  (testing "multiple"
    (is (= '(foo 2 (rksm.cloxp-trace/capture "a" [{:x (rksm.cloxp-trace/capture "b" (+ 3 4))}]))
         (t/tfm-for-capture '(foo 2 [{:x (+ 3 4)}]) [["a" 3] ["b" 6]])))))


(deftest capture-install
  
  (testing "into def"
    (t/reset-captures!) (t/await-captures)
    (let [form '(defn def-for-capture [x] (+ x (- 23 x)))]
      (t/install-capture! form :ns *ns* :name "def-for-capture" :ast-idx 8)
      (is (= 23 (eval '(def-for-capture 3))))
      (is (= {"rksm.cloxp-trace-test/def-for-capture-8" [20]} (t/await-captures)))
      ; (is (= ["rksm.cloxp-trace-test/def-for-capture-7"]
      ;       (:rksm.cloxp-trace/capturing (meta #'def-for-capture))))
      ))
  
  (testing "uninstall"
    (t/reset-captures!) (t/await-captures)
    (let [form '(defn def-for-capture [x] (+ x (- 23 x)))
          spec (t/install-capture! form :ns *ns* :name "def-for-capture" :ast-idx 8)]
      (t/uninstall-capture! (:id spec))
      (is (= 23 (eval '(def-for-capture 3))))
      (is (= {} (t/await-captures)))
      (is (= {} @t/capture-records))
      ))
  
  (testing "re-defining a def keeps captures"
    (let [form '(defn def-for-capture [x] (+ x (- 23 x)))]
      (t/install-capture! form :ns *ns* :name "def-for-capture" :ast-idx 5)
      (eval form)
      (is (= 23 (eval '(def-for-capture 3))))
      (is (= {"rksm.cloxp-trace-test/def-for-capture-5" [23]} (t/await-captures)))
      ))
  
  (testing "multiple captures into same def"
    (t/reset-captures!) (t/await-captures)
    (let [form '(defn def-for-capture [x] (+ x (- 23 x)))]
      (t/install-capture! form :ns *ns* :name "def-for-capture" :ast-idx 5)
      (t/install-capture! form :ns *ns* :name "def-for-capture" :ast-idx 8)
      (is (= 23 (eval '(def-for-capture 3))))
      (is (= {"rksm.cloxp-trace-test/def-for-capture-5" [23],
              "rksm.cloxp-trace-test/def-for-capture-8" [20]} (t/await-captures))))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
 (run-tests 'rksm.cloxp-trace-test)
 )