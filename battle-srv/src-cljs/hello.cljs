(ns hello
   (:require
   [clojure.browser.repl :as repl]))

;;(js/alert "Hello from ClojureScript againxb!")



(defn ^:export connect []
  (js/alert "connect")
  (repl/connect "http://localhost:9001/repl"))
