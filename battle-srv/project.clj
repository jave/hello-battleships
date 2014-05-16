(defproject battle-srv "0.1.0-SNAPSHOT"
  :description "a battleships game server"
  :url "http://example.com/FIXME"
  :license {:name "affero gpl 3"
            :url ""}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.match "0.2.1"]
                 [http-kit "2.1.16"]
                 [compojure "1.1.6"]
                 [org.clojure/tools.cli "0.2.4"]
                 [ring/ring-devel "1.2.2"]
                 [ring/ring-core "1.2.2"]
                 [ring/ring-json "0.3.1"]
                 [javax.servlet/servlet-api "2.5"]
                 [org.clojure/data.json "0.2.4"]
                  [org.clojure/clojurescript "0.0-2197"
                  :exclusions [org.apache.ant/ant]]
                 ]
  :plugins [[lein-cljsbuild "1.0.3"]]
  :cljsbuild {
              :builds [{
                        ;; The path to the top-level ClojureScript source directory:
                        :source-paths ["src-cljs"]
                        ;; The standard ClojureScript compiler options:
                        ;; (See the ClojureScript compiler documentation for details.)
                        :compiler {
                                   :output-to "static/main.js"  ; default: target/cljsbuild-main.js
                                   :optimizations :whitespace
                                   :pretty-print true}}]}
  
  :main battle-srv.core)
