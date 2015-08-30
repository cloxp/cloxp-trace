(ns rksm.cloxp-trace.tracing-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-trace.tracing :refer :all]
            [clojure.string :refer [split-lines]]))

(defn bar
  [x]
  (/ x 2))

(defn foo
  [x]
  (+ 1 (bar x)))

(deftest tracing-test
  
  (testing "printed frames"
    (let [frames (trace [(ns-name *ns*)] (foo 4))
          printed (with-out-str (->> frames printed-in-call-order print))
          expected "| rksm.cloxp-trace.tracing-test/foo [4]\n|  => 3\n| rksm.cloxp-trace.tracing-test/bar [4]\n|  => 2\n"]
      (is (= printed expected))))
  
  (testing "on error"
    (let [frames (trace [(ns-name *ns*)] (foo "failure"))
          printed (with-out-str (->> frames
                                  (map #(update % :result (partial re-find #"^[^\s]+")))
                                  printed-in-call-order print))
          expected "| rksm.cloxp-trace.tracing-test/foo [\"failure\"]\n|  => java.lang.ClassCastException:\n| rksm.cloxp-trace.tracing-test/bar [\"failure\"]\n|  => java.lang.ClassCastException:\n"]
      (is (= printed expected)))))

(comment
 (run-tests *ns*)
 )