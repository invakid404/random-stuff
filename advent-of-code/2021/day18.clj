(require '[clojure.string :as str]
         '[clojure.zip :as zip])

(defn iterate-while [f x pred]
  (->> (iterate f x)
       (take-while pred)
       last))

(defn get-leaf [fish f]
  (loop [curr-fish fish]
    (let [next-fish (f curr-fish)]
      (cond
        (or (nil? next-fish) (zip/end? next-fish)) nil
        (int? (zip/node next-fish)) next-fish
        :else (recur next-fish)))))

(defn next-leaf [fish]
  (get-leaf fish zip/next))

(defn prev-leaf [fish]
  (get-leaf fish zip/prev))

(defn explode-left [[l r] fish]
  (if-let [prev-fish (prev-leaf fish)]
    (next-leaf (zip/edit prev-fish + l))
    fish))

(defn explode-right [[l r] fish]
  (if-let [next-fish (next-leaf fish)]
    (prev-leaf (zip/edit next-fish + r))
    fish))

(defn explode [fish]
  (let [node (zip/node fish)]
    (->> (zip/replace fish 0)
         (explode-left node)
         (explode-right node))))

(defn split-fish [fish]
  (zip/edit
   fish
   (fn [value]
     (->> (/ value 2)
          ((juxt #(Math/floor %) #(Math/ceil %)))
          (mapv int)))))

(defn find-node [pred fish]
  (cond
    (zip/end? fish) nil
    (pred fish) fish
    :else (recur pred (zip/next fish))))

(defn find-exploder [fish]
  (find-node
   (fn [curr]
     (and
      (= 4 (count (zip/path curr)))
      (vector? (zip/node curr))))
   fish))

(defn find-splitter [fish]
  (find-node
   (fn [curr]
     (let [node (zip/node curr)]
       (and
        (int? node)
        (>= node 10))))
   fish))

(defn reduce-fish [fish]
  (if-let [exploder (find-exploder fish)]
    (recur (iterate-while zip/up (explode exploder) some?))
    (if-let [splitter (find-splitter fish)]
      (recur (iterate-while zip/up (split-fish splitter) some?))
      (zip/root fish))))

(defn add-fish [a b]
  (->> (vector a b)
       zip/vector-zip
       reduce-fish))

(defn magnitude [x]
  (if (int? x)
    x
    (let [[l r] x]
      (+ (* 3 (magnitude l))
         (* 2 (magnitude r))))))

(def input
  (->> (slurp "input.txt")
       str/split-lines
       (map read-string)))

(defn part1 [input]
  (->> (reduce add-fish input)
       magnitude))

(defn part2 [input]
  (->> (for [x input
             y input
             :when (not= x y)]
         (magnitude (add-fish x y)))
       (apply max)))

(print (part1 input) (part2 input))

