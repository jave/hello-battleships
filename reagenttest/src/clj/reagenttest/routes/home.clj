(ns reagenttest.routes.home
  (:require [reagenttest.layout :as layout]
            [compojure.core :refer [defroutes GET POST]]
            [ring.util.http-response :as response]
            [clojure.java.io :as io]
            [taoensso.sente :as sente] 
            [taoensso.sente.server-adapters.http-kit      :refer (sente-web-server-adapter)]

            ))

(defn home-page []
  (layout/render "home.html"))

;;for sente
(let [{:keys [ch-recv send-fn ajax-post-fn ajax-get-or-ws-handshake-fn
              connected-uids]}
      (sente/make-channel-socket! sente-web-server-adapter {})]
  (def ring-ajax-post                ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk                       ch-recv) ; ChannelSocket's receive channel
  (def chsk-send!                    send-fn) ; ChannelSocket's send API fn
  (def connected-uids                connected-uids) ; Watchable, read-only atom
  )


;;;; Sente event handlers

(defmulti -event-msg-handler
  "Multimethod to handle Sente `event-msg`s"
  :id ; Dispatch on event-id
  )

(defn event-msg-handler
  "Wraps `-event-msg-handler` with logging, error catching, etc."
  [{:as ev-msg :keys [id ?data event]}]
  (-event-msg-handler ev-msg) ; Handle event-msgs on a single thread
  ;; (future (-event-msg-handler ev-msg)) ; Handle event-msgs on a thread pool
  )

(defmethod -event-msg-handler
  :default ; Default/fallback case (no other matching handler)
  [{:as ev-msg :keys [event id ?data ring-req ?reply-fn send-fn]}]
  (let [session (:session ring-req)
        uid     (:uid     session)]
    (println "Unhandled event: %s" event)
    (when ?reply-fn
      (println "reply-fn")
      (?reply-fn {:umatched-event-as-echoed-from-server event :andsomeofmyown "stuff"}))))

;; TODO Add your (defmethod -event-msg-handler <event-id> [ev-msg] <body>)s here...

(defmethod -event-msg-handler :chsk/ws-ping [ev-msg]
  ;;(println "ping");;just silence the ping
  )

(defmethod -event-msg-handler :grid/click [ev-msg]
  (println "click %s %s" (:?data ev-msg) (:id (:?data ev-msg)) (:set (:?data ev-msg))  )
  ;; send out the grid state change{}
  (chsk-send! :sente/all-users-without-uid [:grid/setstate { :id (:id (:?data ev-msg))  :state (:set (:?data ev-msg)) }  ]))

;;;; Sente event router (our `event-msg-handler` loop)

(defonce router_ (atom nil))
(defn  stop-router! [] (when-let [stop-f @router_] (stop-f)))
(defn start-router! []
  (stop-router!)
  (reset! router_
    (sente/start-server-chsk-router!
      ch-chsk event-msg-handler)))

(start-router!)

(defroutes home-routes

  ;;for sente
  (GET  "/chsk" req (ring-ajax-get-or-ws-handshake req))
  (POST "/chsk" req (ring-ajax-post                req))
  
  (GET "/" [] (home-page))
  (GET "/docs" [] (response/ok (-> "docs/docs.md" io/resource slurp)))
  ;;(GET "/img/" [] (response/ok (-> "docs/docs.md" io/resource slurp)))

  )

