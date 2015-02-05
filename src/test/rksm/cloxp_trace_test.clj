(ns rksm.cloxp-trace-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-trace :as t]))

(defn def-for-capture [x]
  (+ x (- 23 x)))

(defn capture-reset-fixture [test]

  (let [ns (find-ns 'rksm.cloxp-trace-test)]
    (binding [*ns* ns]
      (test)))
  (t/reset-captures!)
  (t/await-captures)
  )

(use-fixtures :each capture-reset-fixture)

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(deftest capture-function
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

(defonce last-meta (atom []))

(deftest capture-install-into-def

  (let [form '(defn def-for-capture [x] (+ x (- 23 x)))]
    (eval form)
    (let [spec (t/install-capture! (pr-str form) :ns *ns* :name "def-for-capture" :ast-idx 8)]
      (is (= {:column 32 :line 1 :end-column 40 :end-line 1} (:pos spec))))
    (is (= 23 (def-for-capture 3)))
    (is (= {"rksm.cloxp-trace-test/def-for-capture-8" [20]} (t/await-captures)))
    (is (= {:hash (hash form)}
           (:rksm.cloxp-trace/capturing (meta #'def-for-capture))))
    (is (= (pr-str form)
           (:source (meta #'def-for-capture)))))
  )

(deftest capture-uninstall-from-def

  (let [form '(defn def-for-capture [x] (+ x (- 23 x)))
        spec (t/install-capture! (pr-str form) :ns *ns* :name "def-for-capture" :ast-idx 8)]
    (t/uninstall-capture! (:id spec))
    (is (= 23 (eval '(def-for-capture 3))))
    (is (= {} (t/await-captures)))
    (is (= {} @t/capture-records))
    ))

(comment
 (-> #'rksm.cloxp-trace-test/def-for-capture .getWatches)
 (-> #'rksm.cloxp-trace/re-install-on-redef (remove-watch :cloxp-capture-reinstall))
 @rksm.cloxp-trace/storage
 (-> #'rksm.cloxp-trace/re-install-on-redef .getWatches)
 )


(deftest redefining-def-keeps-captures

  (let [form '(defn def-for-capture [x] (+ x (- 23 x)))]

    (testing "setup"

      (eval form)
      (t/install-capture! (pr-str form) :ns *ns* :name "def-for-capture" :ast-idx 5)
      (is (= (-> @t/capture-records keys) '("rksm.cloxp-trace-test/def-for-capture-5")))
      (is (= 23 (def-for-capture 3)))
      (is (= {"rksm.cloxp-trace-test/def-for-capture-5" [23]} (t/await-captures)))
      )

    (testing "re-install"
      (eval form)
      (alter-meta! #'def-for-capture assoc :source (pr-str form))
      (Thread/sleep 400)
      (is (= 23 (def-for-capture 3)))
      (is (= {"rksm.cloxp-trace-test/def-for-capture-5" [23 23]} (t/await-captures))))

    (testing "uninstall really removes capture"
      (t/uninstall-capture! "rksm.cloxp-trace-test/def-for-capture-5")
      (t/reset-storage!)
      (is (= 23 (def-for-capture 3)))
      (is (= {} (t/await-captures)))
      (eval form)
      (Thread/sleep 400)
      (is (= 23 (def-for-capture 3)))
      (is (= {} (t/await-captures)))
      )
    ))

(deftest source-changes-disable-reinstall

  (let [form '(defn def-for-capture [x] (+ x (- 23 x)))]

    (testing "setup"

      (eval form)
      (t/install-capture! (pr-str form) :ns *ns* :name "def-for-capture" :ast-idx 5)
      (is (= (-> @t/capture-records keys) '("rksm.cloxp-trace-test/def-for-capture-5")))
      (is (= 23 (def-for-capture 3)))
    ;   (is (= {"rksm.cloxp-trace-test/def-for-capture-5" [23]} (t/await-captures)))
      )

    (testing "re-install"
      (t/reset-storage!)
      (let [new-src "(defn def-for-capture [x] (* x x x))"]
        (eval (read-string new-src))
        (alter-meta! #'def-for-capture assoc :source new-src)
        (Thread/sleep 400)
        (is (= 27 ((deref #'def-for-capture) 3)))
        ; (is (= (-> @t/capture-records keys) '("rksm.cloxp-trace-test/def-for-capture-5")))
        (is (= {} (t/await-captures)))))

    ))

(deftest multiple-captures-into-same-def

  (let [form '(defn def-for-capture [x] (+ x (- 23 x)))]
    (t/install-capture! (pr-str form) :ns *ns* :name "def-for-capture" :ast-idx 5)
    (t/install-capture! (pr-str form) :ns *ns* :name "def-for-capture" :ast-idx 8)
    (is (= 23 (eval '(def-for-capture 3))))
    (is (= {"rksm.cloxp-trace-test/def-for-capture-5" [23],
            "rksm.cloxp-trace-test/def-for-capture-8" [20]} (t/await-captures)))))


; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(deftest install-via-position
  (testing "into def"
    (let [form '(defn def-for-capture [x] (+ x (- 23 x)))]
      (t/install-capture! (pr-str form) :ns *ns* :name "def-for-capture" :pos {:column 32 :line 1})
      (is (= 23 (def-for-capture 3)))
      (Thread/sleep 400)
      (is (= {"rksm.cloxp-trace-test/def-for-capture-8" [20]} (t/await-captures)))
      (is (= {:hash (hash form)}
             (:rksm.cloxp-trace/capturing (meta #'def-for-capture))))
      )))

(deftest max-capture-count
  (let [form '(defn def-for-capture [x] (+ x (- 23 x)))]
    (t/install-capture! (pr-str form) :ns *ns* :name "def-for-capture" :pos {:column 32 :line 1})
    (dotimes [i (+ 5 t/*max-capture-count*)]
      (def-for-capture i))
    (is (= (range -11 19)
           ((t/await-captures) "rksm.cloxp-trace-test/def-for-capture-8")))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
 (t/reset-captures!)
 (ns-unmap *ns* 'def-for-capture)
 (keys (ns-interns *ns*))
 
 (test-vars [#'capture-transform])
 (test-vars [#'redefining-def-keeps-captures])
 (test-vars [#'capture-uninstall-from-def])
 (test-vars [#'install-via-position])
 (test-vars [#'capture-install-into-def])
 (test-vars [#'capture-reset-fixture])
 (test-vars [#'capture-function])
 (test-vars [#'multiple-captures-into-same-def])
 
 (test-vars [#'source-changes-disable-reinstall])

 (t/await-captures)

 (run-tests 'rksm.cloxp-trace-test)
 (eval form)

 )
