(ns reagenttest.env
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init
   (fn []
     (log/info "\n-=[reagenttest started successfully]=-"))
   :stop
   (fn []
     (log/info "\n-=[reagenttest has shutdown successfully]=-"))
   :middleware identity})
