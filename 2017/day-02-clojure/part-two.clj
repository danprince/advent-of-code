;; http://adventofcode.com/2017/day/2

(def sort-by-largest
  (comp reverse sort))

(defn find-even-divisions [[x & ys]]
  (concat
    (for [y ys :let [z (/ x y)] :when (integer? z)] z)
    (when (not-empty ys) (find-even-divisions ys))))

(defn checksum [sheet]
  (->> sheet
    (map sort-by-largest)
    (map find-even-divisions)
    (flatten)
    (reduce +)))

(def sheet [[5 9 2 8]
            [9 4 7 3]
            [3 8 6 5]])

(assert (= (checksum sheet) 9))
