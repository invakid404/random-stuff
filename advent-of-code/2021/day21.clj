(def die-seq
  (->> (range)
       (map #(mod % 100))
       (map inc)))

(defn part1
  ([p1 p2] (part1 p1 p2 0 0 die-seq 0))
  ([p1 p2 s1 s2 die rolls]
   (let [roll (reduce + (take 3 die))
         p1 (-> (+ p1 roll -1) (mod 10) inc)
         s1 (+ s1 p1)]
     (if (>= s1 1000)
       (* s2 (+ rolls 3))
       (recur p2 p1 s2 s1 (drop 3 die) (+ rolls 3))))))

(def quantum-game
  (memoize
   (fn [p1 p2 s1 s2]
     (if (>= s2 21)
       [0 1]
       (reduce (partial mapv +)
               (for [roll1 (range 1 4)
                     roll2 (range 1 4)
                     roll3 (range 1 4)
                     :let [roll (+ roll1 roll2 roll3)
                           p1 (inc (mod (+ p1 roll -1) 10))
                           s1 (+ s1 p1)]]
                 (reverse (quantum-game p2 p1 s2 s1))))))))

(defn part2 [p1 p2]
  (->> (quantum-game p1 p2 0 0)
       (apply max)))

(print (part1 6 2) (part2 6 2))
