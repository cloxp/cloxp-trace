(ns rksm.cloxp-trace-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-trace :as t]))

(defn capture-reset-fixture [test]
  (test) 
  (t/reset-captures!)
  (t/await-captures))

(use-fixtures :each capture-reset-fixture)

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(deftest capture-function
  
  (testing "captures values"
    (do
      (is (= 3 (t/capture "no-loc" (+ 1 2))))
      (is (= {"no-loc" [3]} (t/await-captures)))
      ))
  )

(deftest capture-install

  (testing "form transform"
    (let [form '(/ (apply + a) (count (if (zero? (count a)) Double/POSITIVE_INFINITY a)))]  
      (is (= '(/ (apply + a) (rksm.cloxp-trace/capture "test" (count (if (zero? (count a)) Double/POSITIVE_INFINITY a))))
             (t/tfm-for-capture form {:ast-idx 6 :id "test"})))))
   
  (testing "into def"
    (let [form '(defn def-for-capture [x] (+ x (- 23 x)))]
      (t/install-capture! form :ns *ns* :name "in-test" :ast-idx 7)
      (is (= 23 (eval '(def-for-capture 3))))
      (is (= {"in-test-7" [20]} (t/await-captures)))
      ))
  
  (testing "uninstall"
    (t/reset-captures!) (t/await-captures)
    (let [form '(defn def-for-capture [x] (+ x (- 23 x)))
          spec (t/install-capture! form :ns *ns* :name "in-test" :ast-idx 7)]
      (t/uninstall-capture! (:id spec))
      (is (= 23 (eval '(def-for-capture 3))))
      (is (= {} (t/await-captures)))
      (is (= {} @t/capture-records))
      ))
  
  )

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
 (run-tests 'rksm.cloxp-trace-test)
 )