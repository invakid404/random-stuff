(require '[clojure.string :as str])

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (mean [bottom-val top-val])))))

(defn floor [n]
  (int (Math/floor n)))

(defn ceil [n]
  (int (Math/ceil n)))

(defn part1 [data]
  (let [median-value (median data)]
    (->> (map #(- % median-value) data)
         (map #(Math/abs %))
         (reduce +))))

(defn fuel [n]
  (/ (* n (+ n 1)) 2))

(defn solve2 [data value]
  (->> (map #(- % value) data)
       (map #(Math/abs %))
       (map fuel)
       (reduce +)))

(defn part2 [data]
  (let [mean-value (mean data)]
    (->> (vector (floor mean-value) (ceil mean-value))
         set
         (map #(solve2 data %))
         (apply min))))

(def input
  (as-> (slurp "input.txt") data
        (str/trim data)
        (str/split data #",")
        (map #(Integer. %) data)))

(println (part1 input) (part2 input))
