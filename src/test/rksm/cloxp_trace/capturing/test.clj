(ns rksm.cloxp-trace.capturing.test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-trace.capturing :as t]))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; test data

(defn def-for-capture [x]
  (+ x (- 23 x)))

(defmulti foo-method class)

(defmethod foo-method Number
  [x]
  (+ 3 x))

(defmethod foo-method String
  [x]
  (.toUpperCase x))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn capture-reset-fixture [test]
  (binding [*ns* (find-ns 'rksm.cloxp-trace.capturing.test)]
    (test))
  (t/reset-captures!)
  (t/await-captures))

(use-fixtures :each capture-reset-fixture)

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(deftest capture-function
  (testing "value and trace are captured"
    (is (= 3 (t/capture "no-loc" (+ 1 2))))
    (let [[{v :value, t :trace}] (get (t/await-captures) "no-loc")]      
      (is (= 3 v))
      (is ["rksm.cloxp-trace.capturing.test" nil nil "clojure.core" "clojure.main"]
          (-> (t/await-captures) (get "no-loc") first :trace (->> (take 5) (map :ns))))
      ; (is (= 23 (count t)))
      )))


(deftest capture-transform

  (testing "single"
    (is (= '(/ (apply + a) (rksm.cloxp-trace.capturing/capture "test" (count (if (zero? (count a)) Double/POSITIVE_INFINITY a))))
           (t/tfm-for-capture '(/ (apply + a) (count (if (zero? (count a)) Double/POSITIVE_INFINITY a))) [["test" 6]]))))

  (testing "multiple"
    (is (= '(foo 2 (rksm.cloxp-trace.capturing/capture "a" [{:x (rksm.cloxp-trace.capturing/capture "b" (+ 3 4))}]))
           (t/tfm-for-capture '(foo 2 [{:x (+ 3 4)}]) [["a" 3] ["b" 6]])))))

(deftest capture-install-into-def

  (let [src "(defn def-for-capture [x] (+ x (- 23 x)))"]
    (eval (read-string src))
    (let [spec (t/install-capture! src :ns *ns* :name "def-for-capture" :ast-idx 8)]
      (is (= {:column 32 :line 1 :end-column 40 :end-line 1} (:pos spec)))
      (is (=  "defn" (:type spec))))
    (is (= 23 (def-for-capture 3)))
    
    (let [{[{v :value}] "rksm.cloxp-trace.capturing.test/def-for-capture-8"} (t/await-captures)]
      (is (= 20 v)))
    (is (= {:hash (hash src)}
           (:rksm.cloxp-trace.capturing/capturing (meta #'def-for-capture))))
    (is (= src (:source (meta #'def-for-capture))))))

(deftest capture-uninstall-from-def

  (let [form '(defn def-for-capture [x] (+ x (- 23 x)))
        spec (t/install-capture! (pr-str form) :ns *ns* :name "def-for-capture" :ast-idx 8)]
    (t/uninstall-capture! (:id spec))
    (is (= 23 (eval '(def-for-capture 3))))
    (is (= {} (t/await-captures)))
    (is (= {} @t/capture-records))))

(comment
 (-> #'rksm.cloxp-trace.capturing.test/def-for-capture .getWatches)
 (-> #'rksm.cloxp-trace.capturing/re-install-on-redef (remove-watch :cloxp-capture-reinstall))
 @rksm.cloxp-trace.capturing/storage
 (-> #'rksm.cloxp-trace.capturing/re-install-on-redef .getWatches)
 )


(deftest redefining-def-keeps-captures

  (let [src "(defn def-for-capture [x] (+ x (- 23 x)))"]

    (testing "setup"
      (eval (read-string src))
      (t/install-capture! src :ns *ns* :name "def-for-capture" :ast-idx 5)
      (is (= (-> @t/capture-records keys) '("rksm.cloxp-trace.capturing.test/def-for-capture-5")))
      (is (= 23 (def-for-capture 3)))
      (is (= [23] (map :value (get (t/await-captures) "rksm.cloxp-trace.capturing.test/def-for-capture-5"))))
      )

    (testing "re-install"
      (eval (read-string src))
      (alter-meta! #'def-for-capture assoc :source src)
      (Thread/sleep 400)
      (is (= 23 (def-for-capture 3)))
      (is (= [23 23] (->> "rksm.cloxp-trace.capturing.test/def-for-capture-5"
                      (get (t/await-captures))
                      (map :value)))))

    (testing "uninstall really removes capture"
      (t/uninstall-capture! "rksm.cloxp-trace.capturing.test/def-for-capture-5")
      (is (= 23 (def-for-capture 3)))
      (is (= {} (t/await-captures)))
      (eval (read-string src))
      (is (= 23 (def-for-capture 3)))
      (is (= {} (t/await-captures))))
    ))

(deftest source-changes-disable-reinstall
  
  (let [src "(defn def-for-capture [x] (+ x (- 23 x)))"]
    
    (testing "setup"      
      (eval src)
      (t/install-capture! src :ns *ns* :name "def-for-capture" :ast-idx 5)
      (is (= (-> @t/capture-records keys) '("rksm.cloxp-trace.capturing.test/def-for-capture-5")))
      (is (= 23 (def-for-capture 3)))
      ;   (is (= {"rksm.cloxp-trace.capturing.test/def-for-capture-5" [23]} (t/await-captures)))
      )
    
    (testing "re-install"
      (t/reset-storage!)
      (let [new-src "(defn def-for-capture [x] (* x x x))"]
        (eval (read-string new-src))
        (alter-meta! #'def-for-capture assoc :source new-src)
        (Thread/sleep 400)
        (is (= 27 ((deref #'def-for-capture) 3)))
        ; (is (= (-> @t/capture-records keys) '("rksm.cloxp-trace.capturing.test/def-for-capture-5")))
        (is (= {} (t/await-captures)))))))

(deftest multiple-captures-into-same-def
  
  (let [form '(defn def-for-capture [x] (+ x (- 23 x)))]
    (t/install-capture! (pr-str form) :ns *ns* :name "def-for-capture" :ast-idx 5)
    (t/install-capture! (pr-str form) :ns *ns* :name "def-for-capture" :ast-idx 8)
    (testing "install"
      (is (= 23 (eval '(def-for-capture 3))))
      (let [{c1 "rksm.cloxp-trace.capturing.test/def-for-capture-5",
             c2 "rksm.cloxp-trace.capturing.test/def-for-capture-8"}
            (t/await-captures)]
        (is (= [23] (map :value c1)))
        (is (= [20] (map :value c2)))))
    (testing "uninstall"
      (t/uninstall-captures-in-def! "rksm.cloxp-trace.capturing.test/def-for-capture-5")
      (is (= {} @t/capture-records)))))


; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(deftest install-via-position

  (testing "into def"
    (let [src "(defn def-for-capture [x] (+ x (- 23 x)))"]
      (t/install-capture! src :ns *ns* :name "def-for-capture" :pos {:column 32 :line 1})
      (is (= 23 (def-for-capture 3)))
      (Thread/sleep 400)
      (is (= [20] (map :value (get (t/await-captures) "rksm.cloxp-trace.capturing.test/def-for-capture-8"))))
      (is (= {:hash (hash src)}
             (:rksm.cloxp-trace.capturing/capturing (meta #'def-for-capture)))))))

(deftest max-capture-count
  (let [src "(defn def-for-capture [x] (+ x (- 23 x)))"]
    (t/install-capture! src :ns *ns* :name "def-for-capture" :pos {:column 32 :line 1})
    (dotimes [i (+ 5 t/*max-capture-count*)]
      (def-for-capture i))
    (let [{captured "rksm.cloxp-trace.capturing.test/def-for-capture-8"} (t/await-captures)
          vals (map :value captured)]
      (is (= (range -11 19) vals)))))

(deftest multi-method

  (let [src "(defmethod foo-method String [x] (.toUpperCase x))"
        spec (t/install-capture! src :ns *ns* :name "foo-method" :ast-idx 6)]
    (is (= "defmethod" (:type spec)))
    (is (= 4 (foo-method 1)))
    (is (= "HELLO" (foo-method "hello")))
    (let [{[{v :value}] "rksm.cloxp-trace.capturing.test/foo-method-6-String"} (t/await-captures)]
      (is (= "HELLO" v)))
    (is (contains? @t/capture-records "rksm.cloxp-trace.capturing.test/foo-method-6-String"))
    ))

(deftest two-multi-methods
  
  (let [src-1 "(defmethod foo-method String [x] (.toUpperCase x))"
        src-2 "(defmethod foo-method Number [x] (+ 3 x))"]
    (t/install-capture! src-1 :ns *ns* :name "foo-method" :ast-idx 6)
    (t/install-capture! src-2 :ns *ns* :name "foo-method" :ast-idx 6)
    (is (= 4 (foo-method 1)))
    (is (= "HELLO" (foo-method "hello")))
    (let [{c1 "rksm.cloxp-trace.capturing.test/foo-method-6-String",
           c2 "rksm.cloxp-trace.capturing.test/foo-method-6-Number"}
          (t/await-captures)]
      ; (is (= ["HELLO"] (map :value c1)))
      (is (= [4] (map :value c2))))))

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

 (let [s (java.io.StringWriter.)]
   (binding [*test-out* s] 
     (test-ns *ns*)
     (print (str s))))
 )
