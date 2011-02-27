(defproject clover "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :dev-dependencies
    [[lein-run "1.0.0"]
     [lein-javac "1.2.1-SNAPSHOT"]
     [swank-clojure "1.3.0-SNAPSHOT"]
     [robert/hooke "1.0.2"]
     [lein-daemon "0.2.1"]]
  :compile-path "build/classes"
  :target-dir "build"
  :source-path "src/clj"
  :java-source-path "src/java"
  )
