(ns reagenttest.core

  ;;sente
  (:require-macros
   [cljs.core.async.macros :as asyncm :refer (go go-loop)])

  (:require [reagent.core :as r]
            [clojure.string :as s]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as HistoryEventType]
            [markdown.core :refer [md->html]]
            [reagenttest.ajax :refer [load-interceptors!]]
            [ajax.core :refer [GET POST]]
            ;;sente
            [cljs.core.async :as async :refer (<! >! put! chan)]
            [taoensso.sente  :as sente :refer (cb-success?)]
            ;;formatting
            [goog.string :as gstring]
            [goog.string.format]
            )
  (:import goog.History))

(let [{:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket! "/chsk" ; Note the same path as before
                                  {:type :auto ; e/o #{:auto :ajax :ws}
                                   })]
  (def chsk       chsk)
  (def ch-chsk    ch-recv) ; ChannelSocket's receive channel
  (def chsk-send! send-fn) ; ChannelSocket's send API fn
  (def chsk-state state)   ; Watchable, read-only atom
  )

(declare hex-button)
(declare tri-button)
(declare rect-button)
(declare piece)
(declare clickable)
(declare select-move-form)
(defn nav-link [uri title page collapsed?]
  [:li.nav-item
   {:class (when (= page (session/get :page)) "active")}
   [:a.nav-link
    {:href uri
     :on-click #(reset! collapsed? true)} title]])

(defn navbar []
  (let [collapsed? (r/atom true)]
    (fn []
      [:nav.navbar.navbar-light.bg-faded
       [:button.navbar-toggler.hidden-sm-up
        {:on-click #(swap! collapsed? not)} "☰"]
       [:div.collapse.navbar-toggleable-xs
        (when-not @collapsed? {:class "in"})
        [:a.navbar-brand {:href "#/"} "reagenttest"]
        [:ul.nav.navbar-nav
         [nav-link "#/" "Home" :home collapsed?]
         [nav-link "#/grid" "Grid" :grid collapsed?]         
         [nav-link "#/hex" "Hex" :hex collapsed?]         
         [nav-link "#/tri" "Tri" :tri collapsed?]
         [nav-link "#/anim" "Anim" :anim collapsed?]                  
         [nav-link "#/about" "About" :about collapsed?]]]])))

(defn about-page []
  [:div.container
   [:div.row
    [:div.col-md-12
     "this is the story of reagenttest... work in progress"]]])

(def output (r/atom '[]))

(defn ->output! [fmt & args]
  (let [msg (apply gstring/format (str "%s:" fmt) (js/Date.)  args)]
    (swap! output conj msg)
    ))

(->output! "ClojureScript appears to have loaded correctly.")

(defn lister [items]
                                        ;   (map (fn [x] [:li x]) '("a" "b"))
  [:ul
   (for [item items]
     ^{:key item} [:li  item])])

(defn home-page []
  [:div.container
   [:div.jumbotron
    [:h1 "Welcome to reagenttest"]
    [:p "Time to start building your site!"]

    (lister @output)
    [:p [:a.btn.btn-primary.btn-lg
         {:on-click   #(chsk-send! [:example/button2 {:had-a-callback? "indeed"}] 5000
                                   (fn [cb-reply] (->output! "reply: %s" cb-reply))
                                   )
          }
         "signal"]]

    ]])

;;(def grid2 (r/atom '[1 2 3 4 5 6 7 8 9 10  1 2 3 4 5 6 7 8 9 10  1 2 3 4 5 6 7 8 9 10  1 2 3 4 5 6 7 8 9 10  1 2 3 4 5 6 7 8 9 10  1 2 3 4 5 6 7 8 9 10  1 2 3 4 5 6 7 8 9 10  1 2 3 4 5 6 7 8 9 10  1 2 3 4 5 6 7 8 9 10  1 2 3 4 5 6 7 8 9 10]))
(def grid2 (r/atom  (into [] (doall (take 100 (repeat " "))))))
;;(def grid (r/atom '[[1 2 3][ 4 5 6][ 7 8 9]]))
(def setstate (r/atom "X"))
(declare svg-button)
(defn idxy [x y] (+ x(* y 10)))
(defn getxy [grid x y]
  (nth grid (idxy x y))
  )

(defn grid-button [id label]
  ^{ :key (str "gridkey" id) }  [:a.btn.btn-primary.btn-lg 
                                 { :on-click   #(chsk-send! [:grid/click {:id id :set @setstate }] 5000)}
                                 label
                                 ])

(defn grid-page []
  [:div.container
   [:div.jumbotron
    [:h1 "Grid"]

    ;;when 2d grid
                                        ;    (map (fn [x] (list [:br] (map (fn [y] (grid-button y)) x) )) @grid)
    ;;for 1d mapped to 2d
    ;;(loop [i 0] [:a (nth  @grid i)] (if (= 0 (mod (inc i) 3)) (println "br"))(if (= i 8) 1 (recur (inc i))))
    ;;(map (fn [x] (list [:br] (map (fn [y] (grid-button y)) x) )) @grid)
    (select-move-form)

    ;; loop with a "br" every 10th cell
    ;; (remove nil?
    ;;         (loop [i 0 acc []]
    ;;           (if (= i 100) acc
    ;;               (recur (inc i)
    
    ;;                      (conj acc
    ;;                             ^{:key (str "smurf" i)}  (grid-button i (str "" (nth  @grid2 i))  )
    ;;                             (if (= 0 (mod (inc i) 10)) [:br] )
    ;;                             )
    
    ;;                      ))   ))

    ;;    (into [:tbody ](loop [coll @grid2 acc []]   (if (empty? coll) acc (recur (drop 10 coll) (conj acc (into [:tr ] (mapcat (fn [x] [[:td "(" x ")"]]) (take 10 coll))) )))))

    ;; a table based grid , the grid is just a 1 dimensional vector

    ;; (into [:tbody ]
    ;;       (let [i (r/atom -1)]
    ;;         (loop [coll @grid2
    ;;                acc []]
    ;;           (if (empty? coll) acc
    ;;               (recur (drop 10 coll)
    ;;                      (conj acc (into [:tr ] (mapcat (fn [x] [[:td (do (swap! i inc) (grid-button @i x))]]) (take 10 coll))) ) ;;x no good cell id
    ;;                      ))
    ;;           )))
    
    
    ;;(loop [coll grid acc []]   (if (empty? coll) acc (recur (drop 10 coll) (conj acc (conj [:tr] (into [] (mapcat (fn [x] [[:td x]]) (take 10 coll))) )))))
    ;; [:tbody
    ;;  (loop [coll @grid2 acc []]
    ;;    (if (empty? coll) acc
    ;;        (recur (drop 10 coll)
    ;;               (conj acc [:tr (mapcat (fn [x] [[:td x]]) (take 10 coll)) ]))))
    ;; ]
    ]

   ;;for the svg loop to work i must mysteriously loop over the @grid2 1st, probably lazyness or something
   (loop [coll @grid2
          acc []]
     )
   
   [:svg 
    {:width  1000
     :height  1000
     :style {:border "1px solid black"}}
    [:text {:style {:-webkit-user-select "none"
                    :-moz-user-select "none"}
            :x 20 :y 20 :font-size 20}
     "svg"]
    [:image {
             :x 0;(+ 10 x)
             :y 0;(+ 65 y)
             :width 1000
             :height 1000
             ;;xlinkHref = :xlink:href
             :xlinkHref "/img/bg.svg"
             ;;"https://upload.wikimedia.org/wikipedia/commons/f/fd/Ghostscript_Tiger.svg";;"https://upload.wikimedia.org/wikipedia/commons/e/eb/Hauora.svg"
             }
     ]
    
    
    (for [x (range 0 10)
          y (range 0 10)]
      (rect-button (* 100 x)
                   (* 100 y)
                   (str (getxy @grid2 x y))
                   (idxy x y)))
    
    ]
   ;;   [:div "grid 3: " (nth @grid2 3)]
   ])



(def svgicons  #{"heart" "ring" "cross" "goateye" "eye"  "hand" "cockroach" "skull" "space"})

(defn piece [state wh o]
  (let* [;wh 200
         ;sw 100
         w wh h wh
        ;o sw;(- wh sw)
        from (s/join " " [0 (/ w 2) (/ h 2)] )
        to (s/join " " [360 (/ w 2) (/ h 2)] )]
   [:svg
    {
     :x o :y o
     :width wh :height wh
     :viewBox "0 0 100 100"
      }
    (conj (if (contains? svgicons state)
            [:image
                            {
                             :x 0
                             :y 0
                             :width 100
                             :height 100
                             :xlinkHref
                             (str "/img/" state ".svg" )
                             ;;:transform-origin "150px,150px"
                             }]
   [:text 
    {:style {:-webkit-user-select "none"
             :-moz-user-select "none"


             }
     :fill "red"
     :stroke "black"
     :x 11
     :y 101

     :font-size 100
     }
   
    state

    ]
  )

       [:animateTransform {
                       ;; animatetransform is a bit dumb because for scale its hard to define the transform-origin
                       ;; you can do it easier for rotate
                       :attributeName "transform"
                       :attributeType "XML"
                       :type "rotate"
                       :from "0 50 50"
                       :to "360 50 50"
                       ;; :type "scale"
                       ;; :from "0.1"
                       ;; :to "1"                           
                       ;; :type "matrix"
                       ;; :from (scalematrix 0.1 50 50)
                       ;; :to (scalematrix 1 50 50)

                       :dur "10s"
                       :repeatCount "indefinite"
                       }]

      )])
  )
(defn hex-page []
  [:div.container
   [:div.jumbotron
    [:h1 "Hex"]

    ;;     ;;when 2d grid
    ;;                                         ;    (map (fn [x] (list [:br] (map (fn [y] (grid-button y)) x) )) @grid)
    ;;     ;;for 1d mapped to 2d
    ;;     ;;(loop [i 0] [:a (nth  @grid i)] (if (= 0 (mod (inc i) 3)) (println "br"))(if (= i 8) 1 (recur (inc i))))
    ;;     ;;(map (fn [x] (list [:br] (map (fn [y] (grid-button y)) x) )) @grid)
    (select-move-form)
    ]
   ;;    ;;for the svg loop to work i must mysteriously loop over the @grid2 1st, probably lazyness or something
   (loop [coll @grid2
          acc []]
     )
   
   [:svg 
    {:width  1000
     :height  1000
     :style {:border "1px solid black"}}
    
    
    (for [x (range 0 10)
          y (range 0 10)]

      (hex-button (* 100 x)
                  (+ (* 100 y ) (if (even? x) 0 50))
                  (str (getxy @grid2 x y))
                  (idxy x y)))
    
    ]

   ]
  )

  (defn select-move-form []

    [:div.container
     (into [:form]   (map    (fn [x] [:span (str x) [:input {:type "radio" :name "set" :value 1 :on-change #(reset! setstate x)}]] )
                             (apply sorted-set (into (into svgicons (map  str (range 0 9)))
                                                     (map char (range 65 91))))))
     ]
    )

(defn clickable [id & body]
  (into [:g { ;;    :transform (str "translate(" x "  " y " )" )
       :on-click   #(chsk-send! [:grid/click {:id id :set @setstate }] 5000)
       ;;           :on-mouse-enter (fn [e] (do (.setAttribute (.-target e)   "opacity" 0.5) nil))
                                        ;           :on-mouse-leave (fn [e] (do (.setAttribute (.-target e)   "opacity" 1.0) nil))
       }]
   body
   ))

(defn hex-button [x y state id]
  [:svg {:x x :y y
         :width 100 :height 100
         :viewBox "0 0 300 300"}
   (clickable id
               [:path { :class "hex"
                       :d "M300,150 L225,280 L75,280 L0,150     L75,20   L225,20 Z"
                       ;;:d "M300,150  l-75,130 l-150,0 l-10,-10 l-75,-130  l75,-130 l150,00 l-10-10 z"
                          ;;:stroke-linejoin "round"
                          ;;:style "fill:lime;stroke:purple;stroke-width:1"
                          :stroke-width 4
                          :fill "lightgrey"
                          :stroke "black"
                          }
                ]
               (piece state 300 0)
               )])



(defn tri-page []
  [:div.container
   [:div.jumbotron
    [:h1 "Triangles"]

    (select-move-form)


    ]

   ;;    ;;for the svg loop to work i must mysteriously loop over the @grid2 1st, probably lazyness or something
   (loop [coll @grid2
          acc []]
     )
   
   [:svg 
    {:width  1000
     :height  1000
     :style {:border "1px solid black"}}
    
    
    (for [x (range 0 10)
          y (range 0 10)]

      (tri-button (* 50 x)
                  (* 75 y );(+ (* 100 y ) (if (even? x) 0 50))
                  (str (getxy @grid2 x y))
                  (idxy x y)
                  (if (even? y)
                    (if (even? x) 180 0)
                    (if (even? x) 0 180))
                  ))
    
    ]

   ]
  )



(defn anim-page []
  [:div.container
   [:div.jumbotron
    [:h1 "Anim"]

    ]

   
   [:svg 
    {:width  1000
     :height  1000

     :style {:border "1px solid black" }}
     [:defs [:style { :type "text/css"
                     }
             "<![CDATA[
       #layer2 {
         display:none;
       }
    ]]>"
             ]
      ]    
                [:image
                            {
                             :x 0
                             :y 0
                             :width 1000
                             :height 1000
                             :xlinkHref
                             (str "/img/" "eye" ".svg" )
                             ;;:transform-origin "150px,150px"
                             }]
     ;; id="layer2"
     ;; inkscape:label="lid1"
     ;; style="display:inline">
    ;;display:none
    ;;#layer2 {display:none}
    
    ]

   ]
  )




(defn tri-button [x y state id angle]
  (let [;;str didnt work here for joining the strings, which i found baffling
        trans (s/join " " ["rotate(" angle 165 165 ")"] )

        ]
    [:svg {:x x :y y
           :width 100 :height 100
           :viewBox "0 0 330 330"}
     (clickable id
                 [:g {    :transform trans
                      }
                  [:path {
;;                          :d "M165,50   L35,275  L295,275 Z"
                          :d  "M165,50 l-10,0 l-130,225 l10,10 l260,0 l10,-10 z"
                             ;;:stroke-linejoin "round"
                             ;;:style "fill:lime;stroke:purple;stroke-width:1"
                             :stroke-width 4
                             :fill "lightgrey"
                             :stroke "black"
                             }
                   ]]

                 ;;debug
                 ;;    [:rect  {:x 0 :y 0 :width 330 :height 330 :stroke-width 4 :stroke "black" :fill "none"}]
                 ;;  [:rect  {:x 165 :y 165 :width 1 :height 1 :stroke-width 4 :stroke "black" :fill "white"}]

                 (piece state 150 100)
                 
                 )]))






;;https://github.com/reagent-project/reagent/wiki/Beware-Event-Handlers-Returning-False
(defmacro handler-fn
  ([& body]
   `(fn [~'event] ~@body nil)))  ; always return nil
(defn scalematrix [sx cx cy]
  (str sx " 0 0 " sx " " (- cx(* sx cx))  " " (- cy(* sx cy)) ))
;;"matrix(sx, 0, 0, sy, cx-sx*cx, cy-sy*cy)"

(defn rect-button [x y state id]
  [:g
   {          :on-click   #(chsk-send! [:grid/click {:id id :set @setstate }] 5000)
    ;;           :on-mouse-enter (fn [e] (do (.setAttribute (.-target e)   "opacity" 0.5) nil))
                                        ;           :on-mouse-leave (fn [e] (do (.setAttribute (.-target e)   "opacity" 1.0) nil))

    }
   [:rect {:x x
           :y y
           :stroke "black"
           :width 80
           :height 80
           
           :rx 10
           :ry 10
           :fill "lightgray"


           }]
   [:svg
    {
     :x (- x 10)
     :y (- y 15)
     }
    (piece state 100 00)
    ]]
  )



(def pages
  {:home #'home-page
   :about #'about-page
   :grid #'grid-page
   :hex #'hex-page
   :tri #'tri-page
   :anim #'anim-page})

(defn page []
  [(pages (session/get :page))])

;; -------------------------
;; Routes
(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
  (session/put! :page :home))

(secretary/defroute "/about" []
  (session/put! :page :about))

(secretary/defroute "/grid" []
  (session/put! :page :grid))

(secretary/defroute "/hex" []
  (session/put! :page :hex))

(secretary/defroute "/tri" []
  (session/put! :page :tri))
(secretary/defroute "/anim" []
  (session/put! :page :anim))

;; -------------------------
;; History
;; must be called after routes have been defined
(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
     HistoryEventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

;; -------------------------
;; Initialize app
(defn fetch-docs! []
  (GET (str js/context "/docs") {:handler #(session/put! :docs %)}))

(defn mount-components []
  (r/render [#'navbar] (.getElementById js/document "navbar"))
  (r/render [#'page] (.getElementById js/document "app")))

;;;; Sente event handlers

(defmulti -event-msg-handler
  "Multimethod to handle Sente `event-msg`s"
  :id ; Dispatch on event-id
  )

(defn event-msg-handler
  "Wraps `-event-msg-handler` with logging, error catching, etc."
  [{:as ev-msg :keys [id ?data event]}]
  (-event-msg-handler ev-msg))

(defmethod -event-msg-handler
  :default ; Default/fallback case (no other matching handler)
  [{:as ev-msg :keys [event]}]
  (->output! "Unhandled event: %s" event))

(defmethod -event-msg-handler :chsk/state
  [{:as ev-msg :keys [?data]}]
  (if (:first-open? ?data)
    (->output! "Channel socket successfully established!: %s" ?data)
    (->output! "Channel socket state change: %s" ?data)))

;;? doesnt work
;; (defmethod -event-msg-handler :grid/setstate
;;   [{:as ev-msg :keys [?data]}]
;;     (->output! "grid setstate : id:%s state:%s" (:id ?data) (:state ?data))
;; )


(defmethod -event-msg-handler :chsk/recv
  [{:as ev-msg :keys [?data]}]
  (let [id (js/parseInt (:id (nth ?data 1))) ;;TODO the parseint is kinda lame, it should be possible to pass a typed struct from the server
        state  (:state (nth ?data 1))]
    (->output! "Push event from server:: %s" ?data)
    
    (->output! "setstate: %s %s"   id  state)
    (->output! "grid before: " @grid2)    
    (reset! grid2 (assoc-in @grid2 [id] state))
    (->output! "grid after: " @grid2)
    ))


(defmethod -event-msg-handler :chsk/handshake
  [{:as ev-msg :keys [?data]}]
  (let [[?uid ?csrf-token ?handshake-data] ?data]
    (->output! "Handshake: %s" ?data)))

;; TODO Add your (defmethod -event-msg-handler <event-id> [ev-msg] <body>)s here...

;;;; Sente event router (our `event-msg-handler` loop)
(defonce router_ (atom nil))
(defn  stop-router! [] (when-let [stop-f @router_] (stop-f)))
(defn start-router! []
  (stop-router!)
  (reset! router_
          (sente/start-client-chsk-router!
           ch-chsk event-msg-handler)))


(defn init! []
  (load-interceptors!)
  (fetch-docs!)
  (hook-browser-navigation!)
  (mount-components)
  (start-router!)
  )
