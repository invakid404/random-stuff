(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])

(defn part1 [data]
  (->> (map second data)
       flatten
       (map count)
       (filter #(contains? #{2, 3, 4, 7} %))
       count))

(defn sort-string [s]
  (->> (sort s)
       (str/join "")))

(defn sort-keys [m]
  (reduce-kv (fn [acc curr-key curr-value]
               (update acc (sort-string curr-key) (constantly curr-value)))
             {} m))

(def alphabet "abcdefg")

(def value-map
  (->> {"acedgfb" 8 "cdfbe" 5 "gcdfa" 2 "fbcad" 3 "dab" 7 "cefabd" 9 "cdfgeb" 6 "eafb" 4 "cagedb" 0 "ab" 1}
       sort-keys))

(defn apply-permutation [permutation-map value]
  (map #(get permutation-map %) value))

(defn permute [permutation-map values]
  (map (partial apply-permutation permutation-map) values))

(defn get-value [value]
  (->> (sort-string value)
       (get value-map)))

(defn compute [outputs]
  (->> (map get-value outputs)
       (str/join "")
       (Integer.)))

(defn check-permutation [[signals outputs] permutation]
  (let [permutation-map (zipmap permutation alphabet)
        new-signals (permute permutation-map signals)
        new-outputs (permute permutation-map outputs)
        signal-values (map #(get-value %) new-signals)]
    (if (every? some? signal-values) (compute new-outputs) false)))

(defn brute-permutations [line]
  (->> (combo/permutations alphabet)
       (map #(check-permutation line %))
       (remove false?)
       first))

(defn part2 [data]
  (->> (map brute-permutations data)
       (reduce +)))

(defn parse-line [line]
  (->> (str/split line #" \| ")
       (map #(str/split % #" "))))

(def input
  (as-> (slurp "input.txt") data
        (str/split-lines data)
        (map parse-line data)))

(println (part1 input) (part2 input))
