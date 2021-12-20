(require '[clojure.string :as str])

(def pixels {\. 0 \# 1})

(defn neighbors [[y x]]
  [[(dec y) (dec x)] [(dec y) x] [(dec y) (inc x)]
   [y       (dec x)] [y       x] [y       (inc x)]
   [(inc y) (dec x)] [(inc y) x] [(inc y) (inc x)]])

(defn bits->num [s]
  (Integer/parseInt s 2))

(defn calculate-index [image pos default]
  (->> (neighbors pos)
       (map #(get-in image % default))
       (apply str)
       bits->num))

(defn enhance-image [{:keys [algo image] :as state} default]
  (let [rows (count image)
        cols (count (first image))]
    (vec (for [y (range -1 (inc rows))]
      (vec (for [x (range -1 (inc cols))]
        (->> (calculate-index image [y x] default)
             (nth algo))))))))

(defn default-value [{:keys [algo counter]}]
  ;; If the first bit in the algo is 1, then all empty spaces will become 1 on
  ;; every second enhance.
  (if (zero? (first algo)) 0 (mod counter 2)))

(defn enhance [{:keys [algo image counter] :as state}]
  (let [default (default-value state)]
    (-> state
        (update :counter inc)
        (assoc :image (enhance-image state default)))))

(defn solve [data iterations]
  (-> (nth data iterations)
      :image
      flatten
      frequencies
      (get 1)))

(defn parse-data [[algo & image]]
  {:algo (mapv pixels algo)
   :image (mapv (partial mapv pixels) image)
   :counter 0})

(def input
  (->> (slurp "input.txt")
       str/split-lines
       (remove str/blank?)
       parse-data
       (iterate enhance)))

(print (solve input 2) (solve input 50))
