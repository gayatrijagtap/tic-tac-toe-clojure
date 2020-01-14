(ns tic-tac-toe-clojure.core)
(require '[clojure.set])


(defn get-player-details [player-num, symbol]
  (println (str "Enter player " player-num " name"))
  {:name (read-line) :symbol symbol :moves #{}}
  )

(def winning-combinations #{[1 2 3] [4 5 6] [7 8 9] [1 4 7] [2 5 8] [3 6 9] [1 5 9] [3 5 7]})

(def initial-board (reduce (fn [x y] (assoc x y " ")) (sorted-map) (range 1 10)))

(defn has-won? [moves] (some (fn [winning-combination] (clojure.set/subset? winning-combination moves)) winning-combinations))

(defn has-draw? [board] (not (some (fn [x] (= x " ")) (vals board))))

(defn invalid-move? [move, board] (not (= (get board move) " ")))

(defn print-board
  [board]
  (->> board
       (partition 3)
       (map (fn [x] (clojure.string/join " | " x)))
       (clojure.string/join "\n--+---+--\n")
       (println)))

(defn play-game [current-player next-player board]
  (print-board (vals board))
  (println (str (:name current-player) " play move"))
  (def current-player-move ((comp read-string read-line)))
  (when (invalid-move? current-player-move board) ((println "invalid move.") (play-game current-player next-player board)))
  (def updated-player-moves (conj (:moves current-player) current-player-move))
  (def updated-board (assoc board current-player-move (:symbol current-player)))
  (cond
    (has-won? updated-player-moves) (do (print-board (vals updated-board)) (println (str (:name current-player) " has won!!!")) true)
    (has-draw? board) (do (print-board (vals updated-board)) (println "Game Draw!!!") true)
    :else (recur next-player (assoc current-player :moves updated-player-moves) updated-board)
    ))

(defn start-game []
  (play-game (get-player-details 1 "X") (get-player-details 2 "O") initial-board)
  )

(start-game)