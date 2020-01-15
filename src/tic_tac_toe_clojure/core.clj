(ns tic-tac-toe-clojure.core
  (:use
    [clojure.set
     :only [subset?]]
    [clojure.string
     :only [join]]
    [tic-tac-toe-clojure.constants
     :only [SPACE
            PIPE
            LINE
            X_SYMBOL
            O_SYMBOL]]
    [tic-tac-toe-clojure.configs
     :only [play-move-msg
            enter-player-name-msg
            invalid-move-msg
            game-win-msg
            game-draw-msg]]
    )
  )

(defn get-player-details [player-num, symbol]
  (println (str enter-player-name-msg player-num))
  {:name (read-line) :symbol symbol :moves #{}}
  )

(def winning-combinations #{[1 2 3] [4 5 6] [7 8 9] [1 4 7] [2 5 8] [3 6 9] [1 5 9] [3 5 7]})

(def initial-board (reduce (fn [x y] (assoc x y SPACE)) (sorted-map) (range 1 10)))

(defn has-won? [moves] (some (fn [winning-combination] (clojure.set/subset? winning-combination moves)) winning-combinations))

(defn has-draw? [board] (not (some (fn [x] (= x SPACE)) (vals board))))

(defn invalid-move? [move, board] (not (= (get board move) SPACE)))

(defn print-board
  [board]
  (->> board
       (partition 3)
       (map (fn [x] (clojure.string/join PIPE x)))
       (clojure.string/join LINE)
       (println)))

(defn get-player-move [player board]
  (println (str (:name player) play-move-msg))
  (let [player-move (read-string (read-line))]
    (if (invalid-move? player-move board) (do ((println invalid-move-msg) (get-player-move player board))) player-move)
    )
  )

(defn declare-winner [winner board] (do (print-board (vals board)) (println (str (:name winner) game-win-msg))))

(defn declare-draw [board] (do (print-board (vals board)) (println game-draw-msg)))

(defn append-new-move [player new-move] (conj (:moves player) new-move))

(defn update-board [player new-move board] (assoc board new-move (:symbol player)))

(defn play-game [current-player next-player board]
  (print-board (vals board))
  (let [current-player-move (get-player-move current-player board)
        updated-player-moves (append-new-move current-player current-player-move)
        updated-board (update-board current-player current-player-move board)
        ]
    (cond
      (has-won? updated-player-moves) (declare-winner current-player updated-board)
      (has-draw? updated-board) (declare-draw updated-board)
      :else (recur next-player (assoc current-player :moves updated-player-moves) updated-board)
      )
    )
  )

(defn start-game []
  (play-game (get-player-details 1 X_SYMBOL) (get-player-details 2 O_SYMBOL) initial-board)
  )

(start-game)