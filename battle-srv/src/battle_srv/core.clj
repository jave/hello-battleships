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
   hiccup.page)
  (:require 
   [ring.middleware.reload :as reload]
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
;;(vec (repeat 8 (vec(repeat  4 nil))))

(defn empty-board [x]
  (into [] (doall (repeat x (into [] (doall (repeat x  nil))))))
  )

(defn empty-battleships-board [size p1-sym p2-sym]
  (let [startboard
        (into [] (concat (board-map  (empty-board size) (fn [cell]   (update-in cell [:view] (fn [x] (into #{} (conj x p1-sym)))) ) )
                         (board-map  (empty-board size) (fn [cell]   (update-in cell [:view] (fn [x] (into #{} (conj x p2-sym)))) ) )))

        p1board
        (randomize-entities startboard  #(rand-int size) #(rand-int size) p1-sym 'ship size)

        p2board
        (randomize-entities p1board #(+ size  (rand-int size)) #(rand-int size)  p2-sym 'ship size)

        ]
    p2board

    ))

(defn empty-memory-board [width p1 p2]
  (list2grid width (map  (fn [x] {:view #{} :entities `({:type piece :value ~x})}) (shuffle (mapcat #(repeat 2 %) (range 0 (* width width)))))))

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

(defn cell-contains-any [cell type ]
  (reduce (fn [x y] (or x y))
          false
          (map #(= type (:type %))
               (:entities cell))
          ))

(defmulti new-game (fn [type players size] type  ))

(defmethod new-game :Battleships [type players size]
  {:Game :Battleships
   :board (empty-battleships-board (first size) (first players) (second players))
   :players #{(first players) (second players)}
   :turn 0
   :last-player nil}
  )

(defmethod new-game :Memory [type players size]
  {:Game :Memory
   :board (empty-memory-board (first size) (first players) (second players))
   :players #{(first players) (second players)}
   :turn 0
   :last-player nil}
  )

;;for empty memory board


(defn list2grid [w coll]
  (into [] (reverse (loop [acc [] rest coll]
                      (if (>= w (count rest))
                        acc
                        (recur (conj acc (into [] (take w rest))) (nthnext  rest w))))                   )))
;;(list2grid 4 (shuffle (mapcat #(repeat 2 %) (range 0 9))))



(def all-players
  {'mv {:name "mattias"}
   'jv  {:name "joakim"}
   'th {:name "tomas"}
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

;;using a vector index to identify games is pretty dumb, so maybe use uuids
(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn get-game [game-id]
  (deref (get  @all-games game-id))
  )
(defn get-game-log [game-id num]
  (nth  (deref (get  @all-games-log game-id)) num)
  )

(defn update-game [game-id new-game]
  (send (get @all-games game-id) (fn [x] new-game))
  )

(defn update-game-log [game-id game]
  (send (get @all-games-log game-id) (fn [x] (conj x game)))
  )

;;we need a lobby as well
(defn list-games [])

(defn list-game-types [])

(defn create-game [])

(defn add-player-to-game [])

(defn start-game [])


(defmulti next-player :Game)

(defmethod next-player :Battleships [game]
  (let [players (vec (:players game))
        num-players (count players)]
    (get players (mod (inc (:turn game)) num-players)))
  )

;;TODO just cloned the bs version
(defmethod next-player :Memory [game]
  (let [players (vec (:players game))
        num-players (count players)]
    (get players (mod (inc (:turn game)) num-players)))
  )


;;(next-player (deref(all-games 0)) )

;;(board-view (empty-player-board 10 'p1 'p2) 'p1)
;; (add-entity-at (empty-player-board 10 'p1 'p2) {:type 'ship :owner 'p1} (rand-int 10) (rand-int 10))

;;(loop [board  (empty-player-board 10 'p1 'p2) cnt 10] (if (zero? cnt) board (recur (add-entity-at board {:type 'ship :owner 'p1} (rand-int 10) (rand-int 10)) (dec cnt))))

(defn board-at [board x y]
  (get-in board [x y])
  )

;;(:type (first (board-at demo-board 0 0)))
(defmulti move-shoot-at (fn [game player x y] (:Game game)))

(defmethod  move-shoot-at :Battleships [game player x y]
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

(defmethod  move-shoot-at :Memory [game player x y]
  (let [board (:board game)
        cell  (board-at board x y)
        new-board (-> board
                      (set-viewers-at (:players game) x y)
                      (add-entity-at  {:type 'shot :owner player} x y))]
    
    (cond
     (cell-contains-any cell 'shot)
     {:result '(false "You already turned over that card")}

     (contains? (:view cell) player)
     {:result '(false "Theres no point turning over a card alredy turned")}

     
     true
     {:result '(true "turned a card over")  :new-game (assoc-in game [:board] new-board)})))

;;(move-shoot-at (get-game 0)  'jv 3 7)

(defn make-move [game-id move-fn player x y]
  (let [game (get-game game-id)
        board (:board game)]
    (cond
     (or (> x (count board)) (> y (count (first  board))) )
     '(false "dont make moves outside the board")

     (= player (next-player game))
     '(false "its not your turn")

     true
     (let [
           {:keys [result new-game] :or {new-game nil}}   (apply (resolve move-fn) [game player x y ])
           
           game-after-move
           (if (first result);;only update board if legal move
             (do (-> new-game
                     (update-in  [:turn] inc)
                     ;;              (assoc-in  [:board] new-board)
                     (assoc-in  [:last-player] player)
                     )))
           ]

       (if (first result) ;;only update the board if the move was legal
         (do (update-game game-id game-after-move)
             (update-game-log game-id game-after-move)
             ))
       result))))

;;(make-move 0 'move-shoot-at 'jv 0 7)


;;(update-in demo-board [0 0 :view] (fn [x] (conj x 'p2)))

(defn add-viewer-at [board player x y]
  (update-in board [x y :view] (fn [x] (into #{} (conj x player)))))

(defn set-viewers-at [board players x y]
  (update-in board [x y :view] (fn [x] players)))

(defn add-entity-at [board entity x y]
  (update-in board [x y :entities] (fn [x] (conj x entity))))


(defn player-view-cell [cell player]
  (cond
   (nil? cell) '. ;;empty cell, 
   (some #{player}  (:view cell))
   (match (vec (sort (map #(:type %) (:entities cell))))
          ['ship 'shot] '! ;;burning ship!
          ['shot] '* ;;shot missed!
          ['ship] '= ;;ship
          [] 'w ;;empty water
          ['shot 'battle-srv.core/piece ] (str "V"  (:value (nth  (:entities cell) 1))) ;;this is for memory, the others are for battleships, so this needs some thinks
          :else cell) ;; something else, debug
   :else '? ;;you cant see here
   ))

(defn board-map [board cell-fn]
  (into [] (map (fn [row] (into [] (map cell-fn row)))
                board))
  ) 


(defn board-view [board player]
  ;;something like
  ;;(map (fn [row] (map (fn [cell](first cell)) row)) demo-board)
  (into [] (map (fn [row] (into [] (map (fn [cell] (player-view-cell cell player)
                                          ) row)))
                board))
  ) 


(defn board-view-table [gameid player boardview]
  [:div
   [:ul "board as seen by " player]
   [:ul "**************************************" ]
   [:table 
    (map (fn [x cx] [:tr  (map (fn [y cy] [:td
                                     
                                     (if ( = '? y )
                                       [:a {:href (str "/bs" "/user/" player "/game/" gameid   "/shoot/" cx "/" cy) } y ]
                                       y )
                                     
                                        ]) x (range 0 (count x))) ]) boardview (range 0 (count boardview)))]])
   

(defn game-view [game-id player
                 & {:keys [log] :or {log nil}}]
  (html
   (let [
         game (if log (get-game-log game-id log) (get-game game-id))
         p1 (get  (vec  (:players game)) 0)
         p2 (get  (vec  (:players game)) 1)
         board (:board game)
         bv1 (board-view board p1)
         bv2 (board-view board p2)
         ]
     (html
      [:ul "players : " (:players game)]
      [:ul "turn    : " (:turn game)]
      [:ul "next player : " (next-player game)]

      (if (or (= 'admin player) (= p1 player))
        (board-view-table game-id p1 bv1)
        )
      
      (if (or (= 'admin player) (= p2 player))
        (board-view-table game-id p2 bv2)
        )
      (:ul "END" ))
     ))
  )




(defn hello [x]
  (str  "Hello, Battleships! " x)
  )

(defroutes app-routes
     
  (GET "/hello/:x" [x] (hello x))
  (GET "/helloes" [] ["H" "H" "H"])
  
  (context "/bs/user/:userid/game/:gameid" [userid gameid]
           (GET "/game-view" [] (game-view (read-string  gameid) (symbol  userid)))
           (GET "/game-log/:log-item" [log-item] (game-view (read-string  gameid) (symbol  userid) :log (read-string log-item)))
           (GET "/shoot/:x/:y" [x y]
                (str  (make-move (read-string  gameid)
                                 'battle-srv.core/move-shoot-at
                                 (symbol  userid)
                                 (read-string  x)
                                 (read-string  y)
                                 )
                      (html [:a {:href (str "/bs" "/user/" userid "/game/" gameid   "/game-view") } "return to game view"  ]))
                ))
  
  (files "" {:root "static"})
  (compojure.route/not-found "No Battleships found. Its a trap!")
  )



(defn -main
  "main."
  [& args]
  (let [handler (if true ;(in-dev? args)
                  (reload/wrap-reload (site #'app-routes)) ;; only reload when dev
                  (site app-routes))]
    (run-server handler {:port 7890}))

  )
