(require '[clojure.string :as str])

(defn fold-dot [target-coord value dot]
  (update dot target-coord
          (fn [coord]
            (if (> coord value)
              (- (* value 2) coord)
              coord))))

(defn fold [dots [axis value]]
  (let [target-coord (if (= axis "x") 0 1)]
    (->> (map (partial fold-dot target-coord value) dots)
         set)))

(defn part1 [data]
  (->> data
       first
       count))

(defn max-coords [dots]
  (->> dots
       (apply mapv vector)
       (map (partial apply max))
       (map inc)))

(defn part2 [data]
  (let [dots (last data)
        [max-x max-y] (max-coords dots)]
    (->> (for [y (range max-y)
               x (range max-x)]
           (if (contains? dots [x y]) "#" "."))
         (partition max-x)
         (map (partial str/join ""))
         (str/join "\n"))))

(defn parse-dots [{dots false}]
  (->> dots
       (map #(str/split % #","))
       (map (fn [dot-coords] (mapv #(Integer. %) dot-coords)))
       set))

(defn parse-folds [{folds true}]
  (->> folds
       (map #(str/split % #" "))
       (map last)
       (map #(str/split % #"="))
       (map (fn [[axis value]] [axis, (Integer. value)]))))

(defn fold-all [[dots folds]]
  (->> (reductions fold dots folds)
       rest))

(def input
  (->> (slurp "input.txt")
       str/split-lines
       (remove str/blank?)
       (group-by #(str/includes? % "fold along "))
       ((juxt parse-dots parse-folds))
       fold-all))

(run! println ((juxt part1 part2) input))
