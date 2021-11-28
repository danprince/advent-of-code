(ns advent-of-code.day-06
  (:require [clojure.string :as str]))

(defn cols->rows [input]
  (let [rows (str/split-lines input)
        len (count (first rows))]
    (for [i (range len)]
      (map #(nth % i) rows))))

(defn most-frequent [xs]
  (->> (frequencies xs)
       (apply max-key val)
       first))

(defn least-frequent [xs]
  (->> (frequencies xs)
       (apply min-key val)
       first))

(defn part-1 [input]
  (->> (cols->rows input)
       (map most-frequent)
       str/join))

(defn part-2 [input]
  (->> (cols->rows input)
       (map least-frequent)
       str/join))

(assert (= (part-1 "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar") "easter"))
(assert (= (part-2 "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar") "advent"))

(let [input (slurp "day-06/input.txt")]
  (println "Part 1:" (part-1 input))
  (println "Part 2:" (part-2 input)))
