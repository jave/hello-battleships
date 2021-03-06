(defproject battle-srv "0.1.0-SNAPSHOT"
  :description "a battleships game server"
  :url "http://example.com/FIXME"
  :license {:name "affero gpl 3"
            :url ""}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.match "0.2.1"] ; for ML like pattern matching on conditionals
                 [http-kit "2.1.16"] ;embedded web server
                 [compojure "1.1.6"]
                 [org.clojure/tools.cli "0.2.4"]
                 [ring/ring-devel "1.2.2"]
                 [ring/ring-core "1.2.2"]
                 [ring/ring-json "0.3.1"]
                 [javax.servlet/servlet-api "2.5"]
                 [org.clojure/data.json "0.2.4"]
                 ;;[org.clojure/clojurescript "0.0-2197"
                 ;; :exclusions [org.apache.ant/ant]]
                 [org.clojure/clojurescript "0.0-2138"] ;;the austin web repl wants this particular version
                 [enfocus "2.1.0-SNAPSHOT"];;clojurescript DOM maipulation
                 [cljs-ajax "0.2.3"]
                 ]
  :plugins [[com.cemerick/austin "0.1.3"]
            [lein-cljsbuild "1.0.1"];;this version wanted by austin
            [com.keminglabs/cljx "0.3.2"]]

  :cljx {:builds [{:source-paths ["src-cljx"]
                   :output-path "target/generated/src/clj"
                   :rules :clj}

                  {:source-paths ["src-cljx"]
                   :output-path "target/generated/src/cljs"
                   :rules :cljs}]}
  :cljsbuild {
              :builds [{
                        ;; The path to the top-level ClojureScript source directory:
                        :source-paths ["src-cljs" "target/generated/src/cljs"]
                        ;; The standard ClojureScript compiler options:
                        ;; (See the ClojureScript compiler documentation for details.)
                        :compiler {
                                   :output-to "static/main.js"  ; default: target/cljsbuild-main.js
                                   :optimizations :whitespace
                                   :pretty-print true}}]
              ;; Configure the REPL support; see the README.md file for more details.
              :repl-listen-port 9001 ;;at were using austi, so i think this is not needed
              :repl-launch-commands
                                        ; Launch command for connecting the page of choice to the REPL.
                                        ; Only works if the page at URL automatically connects to the REPL,
                                        ; like http://localhost:3000/repl-demo does.
                                        ; $ lein trampoline cljsbuild repl-launch firefox <URL>
              {"firefox" ["firefox"
                          :stdout ".repl-firefox-out"
                          :stderr ".repl-firefox-err"]
               }
              }
  :hooks [cljx.hooks]
  :main battle-srv.core
  :source-paths [ "src" "target/generated/src/clj"]

  )

