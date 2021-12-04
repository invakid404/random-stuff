(require '[clojure.string :as str])

(defn transpose [m]
  (apply map list m))

(defn check-rows [board]
  (->> (filter (partial every? #(= "x" %)) board)
       seq
       boolean))

(defn check-cols [board]
  (->> (transpose board)
       check-rows))

(defn check-board [board]
  (or (check-rows board) (check-cols board)))

(defn mark-row [number row]
  (map #(if (= number %) "x" %) row))

(defn mark-board [number board]
  (map #(mark-row number %) board))

(defn calculate-board-score [board number]
  (->> (flatten board)
       (filter #(not (= "x" %)))
       (map #(Integer. %))
       (apply +)
       (* number)))

(defn play [{:keys [numbers boards]} win?]
  (let [current-number-str (first numbers)
        current-number (Integer. current-number-str)
        marked-boards (map (partial mark-board current-number-str) boards)
        [winning-boards losing-boards] ((juxt filter remove) check-board marked-boards)
        winning-board (first winning-boards)]
    (cond
      ;; If we're trying to win, end if we've found a winning board
      ;; If we're trying to lose, end if there's only a single winning board left
      (if win? winning-board (and (= (count marked-boards) 1) (seq winning-boards)))
      (calculate-board-score winning-board current-number)
      :else
      (play (hash-map :numbers (rest numbers), :boards (if win? marked-boards losing-boards)) win?))))

(def input
  (as-> (slurp "input.txt") data
        (str/split-lines data)
        (remove str/blank? data)
        (let [[numbers & boards] data
               parsed-numbers (str/split numbers #",")
               parsed-boards (->> (map #(str/split (str/triml %) #"\s+") boards) (partition 5))]
          {:numbers parsed-numbers :boards parsed-boards})))

(println (play input true) (play input false))
