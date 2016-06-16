(ns reagenttest.env
  (:require [selmer.parser :as parser]
            [clojure.tools.logging :as log]
            [reagenttest.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (parser/cache-off!)
     (log/info "\n-=[reagenttest started successfully using the development profile]=-"))
   :stop
   (fn []
     (log/info "\n-=[reagenttest has shutdown successfully]=-"))
   :middleware wrap-dev})
