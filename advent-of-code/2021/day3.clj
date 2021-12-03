(defn most-frequent [items]
  (let [total (count items)
        ones (count (filter #{"1"} items))]
    (if (>= ones (/ total 2)) "1" "0")))

(defn transpose [m]
  (apply mapv vector m))

(defn bits2num [bits]
  (reduce
   #(bit-or
     (bit-shift-left %1 1)
     %2)
   0 bits))

(defn bitvec2num [bits]
  (->> bits
       (map #(Integer/parseInt %))
       bits2num))

(defn part1 [rows]
  (let [columns (transpose rows)
        frequent-bits (->> columns (map most-frequent) (map #(Integer/parseInt %)))
        least-frequent-bits (map (partial bit-xor 1) frequent-bits)
        gamma (bits2num frequent-bits)
        epsilon (bits2num least-frequent-bits)]
    (* gamma epsilon)))

(defn rating
  ([rows freq
    (rating rows freq 0)])
  ([rows freq idx
    (let [columns (transpose rows)]
      n (count columns)
      target-column (nth columns idx)
      frequent-bit (most-frequent target-column)
      least-frequent-bit (if (= frequent-bit "1") "0" "1")
      target-bit (if (= freq 1) frequent-bit least-frequent-bit)
      filtered-rows (filterv #(= (nth % idx) target-bit) rows))
    (if (or (= (count filtered-rows) 1) (>= idx (- n 1)))
      (first filtered-rows)
      (rating filtered-rows freq (inc idx)))]))

(defn part2 [rows]
  (let [oxygen-rating-bits (rating rows 1)
        oxygen-rating (bitvec2num oxygen-rating-bits)
        co2-rating-bits (rating rows 0)
        co2-rating (bitvec2num co2-rating-bits)]
    (* oxygen-rating co2-rating)))

(def input
  (as-> (slurp "input.txt") data
        (clojure.string/split data #"\n")
        (mapv #(clojure.string/split % #"") data)))

(println (part1 input) (part2 input))
