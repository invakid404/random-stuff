(defn simulate
  ([input vx vy] (simulate input vx vy 0 0))
  ([[x1 x2 y1 y2 :as input] vx vy px py]
   (cond
     (or (> px x2) (< py y1)) 0
     (and (>= px x1) (<= py y2)) 1
     :else (recur input
                  (max 0 (dec vx))
                  (dec vy)
                  (+ px vx)
                  (+ py vy)))))

(defn hits [[x1 x2 y1 y2 :as input]]
  (for [x (range 1 (inc x2))
        y (range y1 (- y1))]
    (simulate input x y)))

(defn part1 [[x1 x2 y1 y2]]
  (/ (* y1 (inc y1)) 2))

(defn part2 [input]
  (->> (hits input)
       (reduce +)))

(def input
  (->> (slurp "input.txt")
       (re-seq #"-?\d+")
       (mapv #(Integer. %))))

(print (part1 input) (part2 input))
