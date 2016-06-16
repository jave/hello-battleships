(ns user
  (:require [mount.core :as mount]
            [reagenttest.figwheel :refer [start-fw stop-fw cljs]]
            reagenttest.core))

(defn start []
  (mount/start-without #'reagenttest.core/repl-server))

(defn stop []
  (mount/stop-except #'reagenttest.core/repl-server))

(defn restart []
  (stop)
  (start))


