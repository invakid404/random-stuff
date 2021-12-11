(def deltas-no-diagonals [[-1, 0] [1, 0] [0, -1] [0, 1]])

(def deltas-with-diagonals
  (->> (for [x (range -1 2)
             y (range -1 2)]
         (vector x y))
       (remove (partial every? zero?))))

(defn matrix [m]
  {:values (into [] (flatten m)) :rows (count m) :cols (count (first m))})

(defn index-1d [{:keys [cols]} [x, y]]
  (+ (* x cols) y))

(defn index-2d [{:keys [cols]} idx]
  (vector (quot idx cols) (rem idx cols)))

(defn neighbors [deltas {:keys [rows cols] :as input} idx]
  (let [curr-index (index-2d input idx)]
    (->> deltas
         (map (partial map + curr-index))
         (filter
          (fn [[x, y]]
            (and
             (< -1 x rows)
             (< -1 y cols))))
         (map (partial index-1d input)))))
