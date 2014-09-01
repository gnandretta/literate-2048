(defproject literate-2048 "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2311"]
                 [org.clojure/core.async "0.1.338.0-5c5012-alpha"]
                 [quiescent "0.1.4"]]

  :plugins [[lein-cljsbuild "1.0.4-SNAPSHOT"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "literate-2048"
              :source-paths ["src"]
              :compiler {
                :output-to "literate_2048.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})
