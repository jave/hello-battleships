(ns battle-srv.core

  (:use 
   (compojure [core :only [defroutes context GET POST]]
              [route :only [files]]
              [handler :only [site]])
   [clojure.core.match :only (match)]
                                        ;[clojure.tools.logging :only [info]]
   [clojure.tools.cli :only [cli]]
   ring.middleware.json
   org.httpkit.server
   org.httpkit.timer
   [clojure.tools.cli :only [cli]]
   ring.util.response
   hiccup.core
   hiccup.page
   hiccup.element)
  (:require
   [cemerick.austin.repls :refer (browser-connected-repl-js)]
   [ring.middleware.reload :as reload]
   [clojure.data.json :as json]
                                        ;[clj-http.client :as client]
   [clojure.edn])
  )


(def demo-board
  '[
    [{:view #{p1} :entities ({:type ship :owner p1} )} {:view #{p1}} nil nil]  
    [nil nil nil nil ]
    [nil nil nil nil ]
    [nil nil nil {:view #{p1 p2} :entities ({:type shot :owner p2})} ]

    [{:view #{p2 p1} :entities ({:type ship :owner p2} {:type shot :owner p1} )} nil nil nil]  
    [nil nil nil nil ]
    [nil nil nil nil ]
    [nil nil nil {:view #{p1 p2} :entities ({:type shot :owner p1})} ]
    ])


(declare add-entity-at)
(declare board-map)
(declare randomize-entities)
(declare list2grid)
(declare cell-contains-any)
(declare board-at)
(declare set-viewers-at
         add-viewer-at
         set-entities-at)
(declare all-players all-games all-games-log)
(declare clients send-message)
;;(vec (repeat 8 (vec(repeat  4 nil))))

(defn empty-board [x]
  (into [] (doall (repeat x (into [] (doall (repeat x  nil)))))))

(defn empty-battleships-board [size p1-sym p2-sym]
  (let [startboard
        (into [] (concat (board-map  (empty-board size) (fn [cell]   (update-in cell [:view] (fn [x] (into #{} (conj x p1-sym)))) ) )
                         (board-map  (empty-board size) (fn [cell]   (update-in cell [:view] (fn [x] (into #{} (conj x p2-sym)))) ) )))

        p1board
        (randomize-entities startboard  #(rand-int size) #(rand-int size) p1-sym 'ship size)

        p2board
        (randomize-entities p1board #(+ size  (rand-int size)) #(rand-int size)  p2-sym 'ship size)

        ]
    p2board))

(defn empty-memory-board [width p1 p2]
  (list2grid width (map  (fn [x] {:view #{} :entities `({:type piece :value ~x})})
                         (shuffle (mapcat #(repeat 2 %) (range 0 (/ (* width width) 2)))))))

(defn randomize-entities [startboard xrand yrand owner type startcount]
  (loop [board  startboard
         cnt startcount]
    (if (zero? cnt) board
        (let [x (apply xrand [])
              y (apply yrand [])]
          (if (cell-contains-any (board-at board x y) type)
            (recur board cnt);;already a ship, randomize again
            (recur  (add-entity-at board {:type type :owner owner}
                                   x y
                                   )
                    (dec cnt)))))))
(defn hide-all [board]
  (board-map  board
              (fn [cell]   (update-in cell [:view] (fn [x] #{})) ) )
)

(defn cell-contains-any [cell type ]
  (reduce (fn [x y] (or x y))
          false
          (map #(= type (:type %))
               (:entities cell))))

(defmulti new-game (fn [type players size] type  ))

(defmethod new-game :Battleships [type players size]
  {:Game :Battleships
   :board (empty-battleships-board (first size) (first players) (second players))
   :players #{(first players) (second players)}
   :turn 0
   :player nil})

(defmethod new-game :Memory [type players size]
  {:Game :Memory
   :board (empty-memory-board (first size) (first players) (second players))
   :players #{(first players) (second players)}
   :turn 0
   :turn-phase 'pick-first
   :player (first players) 
   :next-player  (second players)})

;;for empty memory board


(defn list2grid [width coll]
  (into [] (reverse (loop [acc [] rest coll]
                      (if (> width (count rest))
                        acc
                        (recur (conj acc (into [] (take width rest)))
                               (nthnext  rest width))))                   )))
;;(list2grid 4 (shuffle (mapcat #(repeat 2 %) (range 0 9))))




;;using a vector index to identify games is pretty dumb, so maybe use uuids
(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn get-game [game-id]
  (deref (get  @all-games game-id)))

(defn get-game-log [game-id num]
  (nth  (deref (get  @all-games-log game-id)) num))

(defn update-game [game-id new-game]
  (send (get @all-games game-id) (fn [x] new-game)))

(defn update-game-log [game-id game]
  (send (get @all-games-log game-id) (fn [x] (conj x game))))

;;we need a lobby as well
(defn list-games [])

(defn list-game-types [])

(defn create-game [])

(defn add-player-to-game [])

(defn start-game [])


(defmulti next-player :Game)

(defn next-player-in-set  [game]
  (let [players (vec (:players game))
        player-idx (.indexOf   players (:player game))
        num-players (count players)]
    (get players (mod (inc player-idx) num-players))))

(defmethod next-player :Battleships [game]
  (let [players (vec (:players game))
        num-players (count players)]
    (get players (mod (inc (:turn game)) num-players))))


(defmethod next-player :Memory [game]
  (:next-player game))


;;(next-player (deref(all-games 0)) )

;;(board-view (empty-player-board 10 'p1 'p2) 'p1)
;; (add-entity-at (empty-player-board 10 'p1 'p2) {:type 'ship :owner 'p1} (rand-int 10) (rand-int 10))

;;(loop [board  (empty-player-board 10 'p1 'p2) cnt 10] (if (zero? cnt) board (recur (add-entity-at board {:type 'ship :owner 'p1} (rand-int 10) (rand-int 10)) (dec cnt))))

(defn board-at [board x y]
  (get-in board [x y]))

;;(:type (first (board-at demo-board 0 0)))
(defmulti move-pick-cell (fn [game player x y] (:Game game)))

(defmethod  move-pick-cell :Battleships [game player x y]
  (let [board (:board game)
        cell  (board-at board x y)
        new-board (-> board
                      (add-viewer-at player x y)
                      (add-entity-at  {:type 'shot :owner player} x y))]
    
    (cond
     (cell-contains-any cell 'shot)
     {:result '(false "You already made a shot there") }

     (contains? (:view cell) player)
     {:result '(false "Theres no point shooting where you can see already") }

     
     (cell-contains-any cell 'ship)
     {:result '(true "Hit an enemy ship!") :new-game (assoc-in game [:board] new-board)}

     true
     {:result '(true "Missed!")  :new-game (assoc-in game [:board] new-board) })))

(defmethod  move-pick-cell :Memory [game player x y]
  (let [board (:board game)
        cell  (board-at board x y)]
    
    (cond
     (cell-contains-any cell 'shot)
     {:result '(false "You already turned over that card")}

     (contains? (:view cell) player)
     {:result '(false "Theres no point turning over a card alredy turned")}

     
     (or     (= 'pick-first (:turn-phase game))
             (nil? (:turn-phase game) ;;a safeguard
                   ))
     {:result '(true "turned first card over")
      :new-game (-> game
                    (assoc-in [:first-pick] (list x y)) ;;there are any number of ways to do this more generally, but start out like this
                    (assoc-in [:turn-phase] 'pick-second)
                    (assoc-in [:next-player] player)
                    (assoc-in [:card1] (:value (nth  (:entities cell) 0)))
                    (assoc-in [:board]      (set-viewers-at board (:players game) x y)))}

     (and  (= 'pick-second (:turn-phase game))
           (not= (:card1 game) (:value (nth  (:entities cell) 0))) )
     {:result '(true "turned second card over, fail")
      :new-game (-> game
                    (assoc-in [:second-pick] (list x y)) ;;there are any number of ways to do this more generally, but start out like this
                    (assoc-in [:next-player] player)
                    (assoc-in [:turn-phase] 'show-second)
                    (assoc-in [:board]      (set-viewers-at board (:players game) x y)))}

     (and  (= 'pick-second (:turn-phase game))
           (= (:card1 game) (:value (nth  (:entities cell) 0))) )
     (let [new-board2
           (->      (set-viewers-at board (:players game) x y)
               (set-entities-at nil x y)
               (set-entities-at nil (first (:first-pick game )) (second (:first-pick game )) ))]

       {:result '(true "turned second card over, success, capture cards and try again!")
        :new-game (-> game
                      ;;first  capture the pair, dont hide now
                      (assoc-in [:turn-phase] 'pick-first)
                      (assoc-in [:next-player] player)
                      (assoc-in [:board]  new-board2))})
     (and  (= 'show-second (:turn-phase game)))
     {:result '(true "hide cards, next player!")
      :new-game (-> game
                    ;;first  capture the pair, then hide them
                    (assoc-in [:turn-phase] 'pick-first)
                    (assoc-in [:next-player] (next-player-in-set game))
                    (assoc-in [:board]
                              (->      board
                                  (set-viewers-at  #{} (first (:first-pick game )) (second (:first-pick game )) )
                                  (set-viewers-at  #{} (first (:second-pick game )) (second (:second-pick game )) )
                                  ))

                    ;;the problem with hiding everything here, is that captured cards cells also gets hidden
                    ;;on sucess we dont need to hide, because the cards are captured away from the board
                    ;;                    
                    )}

     true
     {:result `(false "hmm. shouldnt get here" ~game)}

     )))


(defn filter-board [board filter-fn]
  (filter filter-fn (flatten board))

  )

(defn memory-pairs [board player]
  ;;pairs the player can see
  ()  (filter-board board (fn [x] (some '#{player}  (:view x))))
  )

;;all cells jv can see in game 0
;;(filter-board (:board (get-game 0)) (fn [x] (some '#{jv}  (:view x))))
;;all cells with value less than 5 (get-in doesnt work on lists, but it does on vectors though
;;(filter-board (:board (get-game 1)) (fn [x] (> 5 (:value (first (get-in x [:entities]))))))

;;(move-pick-cell (get-game 0)  'jv 3 7)

(defn make-move [game-id move-fn player x y]
  (let [game (get-game game-id)
        board (:board game)]
    (cond
     (or (> x (count board)) (> y (count (first  board))) )
     '(false "dont make moves outside the board")

     (not= player (next-player game))
     (list false (str "its not your turn " player " " (next-player game)))

     true
     (let [
           {:keys [result new-game] :or {new-game nil}}   (apply (resolve move-fn) [game player x y ])
           
           game-after-move
           (if (first result);;only update board if legal move
             (do (-> new-game
                     (update-in  [:turn] inc)
                     (assoc-in  [:player] player))))]

       (if (first result) ;;only update the board if the move was legal
         (do (update-game game-id game-after-move)
             (update-game-log game-id game-after-move)
             (println (second result))
             (println clients)
             (send-message (second result))
             ))
       result))))

;;(make-move 0 'move-pick-cell 'jv 0 7)


;;(update-in demo-board [0 0 :view] (fn [x] (conj x 'p2)))

(defn add-viewer-at [board player x y]
  (update-in board [x y :view] (fn [x] (into #{} (conj x player)))))

(defn set-viewers-at [board players x y]
  (update-in board [x y :view] (fn [x] players)))


(defn add-entity-at [board entity x y]
  (update-in board [x y :entities] (fn [x] (conj x entity))))

(defn set-entities-at [board entities x y]
  (update-in board [x y :entities] (fn [x] entities)))

(defn player-view-cell [cell player]
  (cond
   (nil? cell) '. ;;empty cell, 

   (or (= player 'admin) (some #{player}  (:view cell)))
   (match (vec (sort (map #(:type %) (:entities cell))))
          ['ship 'shot] '! ;;burning ship!
          ['shot] '* ;;shot missed!
          ['ship] '= ;;ship
          [] 'w ;;empty water
          ['battle-srv.core/piece ] (str "V"  (:value (nth  (:entities cell) 0))) ;;this is for memory, the others are for battleships, so this needs some thinks
          :else cell) ;; something else, debug

   :else '? ;;you cant see here
   ))

(defn board-map [board cell-fn]
  (into [] (map (fn [row] (into [] (map cell-fn row)))
                board))) 


(defn board-view [board player]
  ;;something like
  ;;(map (fn [row] (map (fn [cell](first cell)) row)) demo-board)
  (into [] (map (fn [row] (into [] (map (fn [cell] (player-view-cell cell player)
                                          ) row)))
                board))) 


(defn board-view-table [gameid player boardview]
  [:div
   [:ul "board as seen by " player]
   [:ul "**************************************" ]
   [:table 
    (map (fn [x cx] [:tr  (map (fn [y cy] [:td
                                     
                                     (if ( = '? y )
                                       [:a {:id (str cx "_" cy) :href (str "/bs" "/user/" player "/game/" gameid   "/shoot/" cx "/" cy) } y ]
                                       y )
                                     
                                        ]) x (range 0 (count x))) ]) boardview (range 0 (count boardview)))]])
   

(defn- run-clojurescript [path init]
  (list
    (include-js path)
    (javascript-tag init)))

(defn game-view [game-id player
                 & {:keys [log] :or {log nil}}]
  (html
   (html5
    [:head
     (include-js "/main.js")
     (javascript-tag
      (str
       ;;       "alert(document.getElementById('message'));"
;;       "alert(window.location.href.replace('http://', 'ws://').replace('/bs/', '/ws/'));"
        "window.socket = new WebSocket(window.location.href.replace('http://', 'ws://').replace('/bs/', '/ws/'));"
        "socket.onopen = function() {"
       "  return console.log('socket opened')"
       "};"

       "socket.onmessage = function(msg) {"
       ;;"alert(document.getElementById('message'));"
       ;;"alert(msg.data);"
       ;;       " document.getElementById('message').replaceWith('mupp');"
       ;; i dont want jquery atm
       ;;"    return document.getElementById('message').replaceWith('<p >' + msg.data + '</p>');"
       "var message=document.getElementById('message'),"
       "messageparent = message.parentNode,"
       "tempDiv = document.createElement('div');"
       "tempDiv.innerHTML = \"<p>\"+msg.data+\"</p/>\";"
       "messageparent.replaceChild(tempDiv, message);"
        "};"
       )
      
      )]
    
    [:body
     ;;(javascript-tag "hello.connect()") ;;this is for the cljsbuild variant of web repl
     [:script (browser-connected-repl-js)]
     
     (let [
           game (if log (get-game-log game-id log) (get-game game-id))
           p1 (get  (vec  (:players game)) 0)
           p2 (get  (vec  (:players game)) 1)
           board (:board game)
           bv1 (board-view board p1)
           bv2 (board-view board p2)
           bv3 (board-view board 'admin)
           ]
       (html
        [:table 
         [:tr [:td "players : "] [:td  (:players game)]]
         [:tr [:td "player : "] [:td (:player game)]]
         [:tr [:td "next player : "] [:td (next-player game)]]
         [:tr [:td "turn phase : "] [:td (:turn-phase game)]]      
         [:tr [:td "turn    : "] [:td (:turn game)]]
         [:tr [:td "msg    : "] [:td        [:p#message "websocket message" ]]]

         ]


        (if (or (= 'admin player) (= p1 player))
          (board-view-table game-id p1 bv1)
          )
        
        (if (or (= 'admin player) (= p2 player))
          (board-view-table game-id p2 bv2)
          )
        (if (or (= 'admin player) )
          (board-view-table game-id 'admin bv3)
          )
        (:ul "END" )))])))


(def all-players
  {'mv {:name "mattias"}
   'jv  {:name "joakim"}
   'th {:name "tomas"}
   'hz (:name "henrik")
   'admin {:name "administrator"}
   }
  )

(def all-games
  (agent 
   [(agent  (new-game :Battleships '(mv jv) '(10))
            )
    (agent  (new-game :Memory '(mv jv) '(4))
            )]))

(def all-games-log
  ;;push game state log here
  (agent
   [(agent [])
    (agent [])])
  )



(defn hello [x]
  (str  "Hello, Battleships! " x))

(def clients (atom {}))

(defn send-message [message]
    (doseq [client (keys @clients)]
      (send! client
             message
             false )
    ))

(defn msg-handler [req]
  (with-channel req channel
    (println channel "websocket connected") ;;needs logging api
    (swap! clients assoc channel true)
    (on-close channel (fn [status]
                        (swap! clients dissoc channel)
                        (println channel "websocket closed, status" status)
                        ))))

(defroutes app-routes
     
  (GET "/hello/:x" [x] (hello x))
  (GET "/helloes" [] ["H" "H" "H"])
  (GET "/ws/*" []  msg-handler)
  (files "" {:root "static"})
  ;;this context is for the debug html ui
  (context "/bs/user/:userid/game/:gameid" [userid gameid]
           (GET "/game-view" [] (game-view (read-string  gameid) (symbol  userid)))
           (GET "/game-log/:log-item" [log-item] (game-view (read-string  gameid) (symbol  userid) :log (read-string log-item)))
           (GET "/shoot/:x/:y" [x y]
                (str    (make-move (read-string  gameid)
                                           'battle-srv.core/move-pick-cell
                                           (symbol  userid)
                                           (read-string  x)
                                           (read-string  y)
                                           )
                        (await (get  @all-games (read-string  gameid)));;this is needed so that the agents settle before reading them next state
                        (game-view (read-string  gameid) (symbol  userid))
                        (html [:a {:href (str "/bs" "/user/" userid "/game/" gameid   "/game-view")} "return to game view"]))))
  ;;this context is for the rest api
  (context "/bsapi/user/:userid/game/:gameid" [userid gameid]
           (GET "/game-view" [] (json/write-str  (get-game (read-string  gameid))))
                     (GET "/shoot/:x/:y" [x y]
                (json/write-str    (make-move (read-string  gameid)
                                           'battle-srv.core/move-pick-cell
                                           (symbol  userid)
                                           (read-string  x)
                                           (read-string  y)))))
  
  (files "" {:root "static"})
  (compojure.route/not-found "No Battleships found. Its a trap!"))



(defn -main
  "main."
  [& args]
  (let [handler (if true ;(in-dev? args)
                  (reload/wrap-reload (site #'app-routes)) ;; only reload when dev
                  (site app-routes))]
    (run-server handler {:port 7890}))
  )
