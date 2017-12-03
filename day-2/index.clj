(defn diff [xs]
  (- (apply max xs) (apply min xs)))

(defn checksum [sheet]
  (->> sheet (map diff) (reduce +)))

(def sheet [[5 1 9 5]
            [7 5 3]
            [2 4 6 8]])

(assert (= (checksum sheet) 18))
