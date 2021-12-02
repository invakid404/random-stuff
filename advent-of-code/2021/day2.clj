(defn step1 [state [command, amt]]
  (case command
    "forward" (update state :x + amt)
    "up" (update state :y - amt)
    "down" (update state :y + amt)))

(defn part1 [rows]
  (let [start {:x 0 :y 0}
        result (reduce step1 start rows)]
    (* (:x result) (:y result))))

(defn step2 [state [command, amt]]
  (case command
    "forward" (-> state (update :x + amt) (update :y + (* amt (:aim state))))
    "up" (update state :aim - amt)
    "down" (update state :aim + amt)))

(defn part2 [rows]
  (let [start {:x 0 :y 0 :aim 0}
        result (reduce step2 start rows)]
    (* (:x result) (:y result))))

(defn parse-row [row]
  (let [[command, amt-str] (clojure.string/split row #" ")
        amt (Integer/parseInt amt-str)]
    (vector command amt)))

(def input
  (as-> (slurp "input.txt") data
        (clojure.string/split data #"\n")
        (map parse-row data)))

(println (part1 input) (part2 input))
