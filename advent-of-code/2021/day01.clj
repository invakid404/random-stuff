(defn zip [skip nums]
  (partition 2 (interleave nums (drop skip nums))))

(defn solve [nums skip]
  (->> nums
       (zip skip)
       (filter #(apply < %))
       (count)))

(def input
  (as-> (slurp "input.txt") data
        (clojure.string/split data #"\n")
        (map #(Integer/parseInt %) data)))

(println (solve input 1) (solve input 3))
