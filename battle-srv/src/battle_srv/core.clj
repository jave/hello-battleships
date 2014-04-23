(ns battle-srv.core)
(use '[clojure.core.match :only (match)])

(defn hello
  "hello battleships."
  [x]
  (println x "Hello, Battleships!"))

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



;;(vec (repeat 8 (vec(repeat  4 nil))))

(defn empty-board [x]
  (into [] (doall (repeat x (into [] (doall (repeat x  nil))))))
  )

(defn empty-player-board [size p1-sym p2-sym]
  (let [startboard
        (into [] (concat (board-map  (empty-board size) (fn [cell]   (update-in cell [:view] (fn [x] (into #{} (conj x p1-sym)))) ) )
                         (board-map  (empty-board size) (fn [cell]   (update-in cell [:view] (fn [x] (into #{} (conj x p2-sym)))) ) )))

        p1board
        (loop [board  startboard cnt 10] (if (zero? cnt) board   (recur  (add-entity-at board {:type 'ship :owner p1-sym} (rand-int size) (rand-int size)) (dec cnt))))

        p2board
        (loop [board  p1board cnt 10] (if (zero? cnt) board   (recur  (add-entity-at board {:type 'ship :owner p2-sym}  (+ size(rand-int size))  (rand-int size)) (dec cnt))))

        ]
    p2board

    ))

(defn new-game [p1 p2]
  {:board (empty-player-board 10 p1 p2)
   :players #{p1 p2}
   :turn 0
   :last-player nil}
  )

(def all-players
  {'mv {:name "mattias"}
   'jv  {:name "joakim"}
   'th {:name "tomas"}})

(def all-games
  [(ref  (new-game 'mv 'jv))])

(defn next-player [game]
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

(defn move-shoot-at [board player x y]
  (let [cell  (board-at board x y)
        new-board (add-entity-at board {:type 'shot :owner player} x y)]
    
    (if (some #(= 'ship (:type %)) (:entities cell))
      {:result 'yay! :new-board new-board}
      {:result 'nope :new-board new-board})))


(defn make-move [game-id move-fn player x y]
  (let [game-ref (get  all-games game-id)
        game @game-ref

        {:keys [result new-board]} (apply (resolve move-fn) [(:board game) player x y ])

        game-after-move
        (-> game
            (update-in  [:turn] inc)
            (assoc-in  [:board] new-board)
            (assoc-in  [:last-player] player)
        )
        ]


    (dosync (ref-set game-ref game-after-move))
    result))

;;(update-in demo-board [0 0 :view] (fn [x] (conj x 'p2)))

(defn add-viewer-at [board player x y]
  (update-in board [x y :view] (fn [x] (into #{} (conj x player)))))

(defn add-entity-at [board entity x y]
  (update-in board [x y :entities] (fn [x] (conj x entity))))

(defn player-view-at [board player x y]
  (let [cell (board-at board x y)]
    (if ((some #{player} ) (:view cell))))
  )


(defn player-view-cell [cell player]
  (cond
   (nil? cell) '. ;;empty cell, 
   (some #{player}  (:view cell))
   (match (vec (sort (map #(:type %) (:entities cell))))
          ['ship 'shot] '! ;;burning ship!
          ['shot] '* ;;shot missed!
          ['ship] '= ;;ship
          :else '_) ;; something else
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

(defn admin-game-view [game-id]
  (let [game-ref (get  all-games game-id)
        game @game-ref
        p1 (get  (vec  (:players game)) 0)
        p2 (get  (vec  (:players game)) 1)
        board (:board game)
        bv1 (board-view board p1)
        bv2 (board-view board p2)
        ]
    (println "players : " (:players game))
    (println "turn    : " (:turn game))
    (println "board as seen by " p1)
    (println "**************************************" )
    (dorun  (map #(println "     " %) bv1))
    (println "**************************************" )
    (println "board as seen by " p2)
    (dorun  (map #(println "     " %) bv2))
    (println "END" )
)
  )
