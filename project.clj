(defproject org.rksm/cloxp-trace "0.1.4-SNAPSHOT"
  :description "Tracing for cloxp."
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.json "0.2.3"]
                 [org.clojure/tools.trace "0.7.8"]
                 [clj-stacktrace/clj-stacktrace "0.2.8"]
                 [org.rksm/cloxp-source-reader "0.1.0-SNAPSHOT"]]
  :source-paths ["src/main"]
  :test-paths ["src/test"]
  :scm {:url "git@github.com:cloxp/cloxp-trace.git"}
  :pom-addition [:developers [:developer
                              [:name "Robert Krahn"]
                              [:url "http://robert.kra.hn"]
                              [:email "robert.krahn@gmail.com"]
                              [:timezone "-9"]]])
