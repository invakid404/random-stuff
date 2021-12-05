(require '[clojure.string :as str])

(def input
  (->> (slurp "input.txt")
       str/split-lines
       (map #(read-string
              (str "[" (str/replace % #",| -> " " ") "]")))))

(defn line-range [n1 n2]
  (if (<= n1 n2)
    (range n1 (inc n2))
    (range n1 (dec n2) -1)))

(defn diagonal-line? [[x1 y1 x2 y2]]
  (not (or (= x1 x2)
           (= y1 y2))))

(defn transpose [m]
  (apply map vector m))

(defn cartesian-product [xs ys]
  (for [x xs
        y ys]
    (vector x y)))

(defn expand-lines [data]
  (for [[x1 y1 x2 y2 :as line] data]
    (let [xs (line-range x1 x2)
          ys (line-range y1 y2)]
      (if (diagonal-line? line)
        (transpose [xs ys])
        (cartesian-product xs ys)))))

(defn get-coords [data]
  (->> data
       expand-lines
       (apply concat)))

(defn solve [coords]
  (->> coords
       frequencies
       (filter #(> (second %) 1))
       count))

(let [coords (get-coords input)
      coords-no-diagonals (get-coords (remove diagonal-line? input))]
  (println (solve coords-no-diagonals) (solve coords)))
