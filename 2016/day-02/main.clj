(ns advent-of-code.day-02
  (:require [clojure.string :as str]))

(def __ nil)

(def expected-keypad
  [[\1 \2 \3]
   [\4 \5 \6]
   [\7 \8 \9]])

(def actual-keypad
  [[__ __ \1 __ __]
   [__ \2 \3 \4 __]
   [\5 \6 \7 \8 \9]
   [__ \A \B \C __]
   [__ __ \D __ __]])

(defn read-key [keypad [x y]]
  (get-in keypad [y x] nil))

(def moves {\L [-1 0]
            \R [1 0]
            \U [0 -1]
            \D [0 1]
            \newline [0 0]})

(defn press [keypad {:keys [pos code]} char]
  (let [prev pos
        vec (get moves char)
        pos (map + prev vec)
        key (read-key keypad pos)]
    (cond
      (= char \newline) {:pos pos, :code (conj code key)}
      (nil? key)        {:pos prev, :code code}
      :else             {:pos pos, :code code})))

(defn part-1 [input]
  (let [press (partial press expected-keypad)]
    (->> input
         (reduce press {:pos [1 1], :code []})
         :code
         (apply str))))

(defn part-2 [input]
  (let [press (partial press actual-keypad)]
    (->> input
         (reduce press {:pos [0 2], :code []})
         :code
         (apply str))))

(assert (= (part-1 "ULL\nRRDDD\nLURDL\nUUUUD\n") "1985"))
(assert (= (part-2 "ULL\nRRDDD\nLURDL\nUUUUD\n") "5DB3"))

(let [input (slurp "day-02/input.txt")]
  (println "Part 1:" (part-1 input))
  (println "Part 2:" (part-2 input)))
