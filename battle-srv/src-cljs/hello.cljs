(ns hello
  (:require [enfocus.core :as ef]
            [enfocus.events :as events]
            [enfocus.effects :as effects]
            [clojure.browser.repl :as repl]
            [ajax.core :refer [GET POST]])
  (:require-macros [enfocus.macros :as em]))

;;(js/alert "Hello from ClojureScript again!")


;;this is used with the lein-cljsbuild brepl, but not with the austin brepl
(defn ^:export connect []
  (js/alert "connect")
  (repl/connect "http://localhost:9001/repl"))

(defn hello
  []
  (js/alert "hello"))

(defn whoami
  []
  (.-userAgent js/navigator))
