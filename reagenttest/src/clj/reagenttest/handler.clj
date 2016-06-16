(ns reagenttest.handler
  (:require [compojure.core :refer [routes wrap-routes]]
            [reagenttest.layout :refer [error-page]]
            [reagenttest.routes.home :refer [home-routes]]
            [compojure.route :as route]
            [reagenttest.env :refer [defaults]]
            [mount.core :as mount]
            [reagenttest.middleware :as middleware]))

(mount/defstate init-app
                :start ((or (:init defaults) identity))
                :stop  ((or (:stop defaults) identity)))

(def app-routes
  (routes
    (-> #'home-routes
        (wrap-routes middleware/wrap-csrf)
        (wrap-routes middleware/wrap-formats)
        ;;the next 2 for sente
        ring.middleware.keyword-params/wrap-keyword-params
        ring.middleware.params/wrap-params)
    (route/not-found
      (:body
        (error-page {:status 404
                     :title "page not found"})))))


(defn app [] (middleware/wrap-base #'app-routes))
