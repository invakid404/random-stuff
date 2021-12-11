(require '[clojure.string :as str])

(load-file "./matrix-utils.clj")

(defn flash-positions [{:keys [values]}]
  (->> (map-indexed vector values)
       (filter (fn [[idx value]] (> value 9)))
       (map first)))

(defn flash-one [{:keys [values] :as state} position]
  (update state :values
          (fn [curr-values]
            (as-> (update curr-values position (constantly 0)) new-values
                  (reduce
                   (fn [acc neighbor]
                     (update acc neighbor
                             (fn [energy]
                               (if (zero? energy) energy (inc energy)))))
                   new-values
                   (neighbors deltas-with-diagonals state position))))))

(defn flash-all [state positions]
  (reduce flash-one state positions))

(defn flash-state [state]
  (let [curr-flash-positions (flash-positions state)]
    (if (empty? curr-flash-positions)
      state
      (recur (flash-all state curr-flash-positions)))))

(defn next-state [state]
  (-> state
      (update :values (partial mapv inc))
      flash-state))

(defn parse-line [line]
  (->> (str/split line #"")
       (map #(Integer. %))))

(defn part1 [data]
  (->> (take 101 data)
       (map :values)
       (map (partial filter zero?))
       (map count)
       (reduce +)))

(defn part2 [data]
  (->> (map :values data)
       (reduce
        (fn [acc state]
          (if (every? zero? state)
            (reduced acc)
            (inc acc)))
        0)))

(def input
  (->> (slurp "input.txt")
       str/split-lines
       (map parse-line)
       matrix
       (iterate next-state)))

(print (part1 input) (part2 input))
