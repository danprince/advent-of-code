(ns day-03
  (:require [clojure.string :as str]))

(def pattern #"\s*(\d+)\s+(\d+)\s+(\d+)\s*")

(defn parse-triangle [s]
  (->> (re-matches pattern s)
       rest
       (map #(Integer/parseInt %))))

(defn parse-triangles [s]
  (->> (str/split-lines s)
       (map parse-triangle)))

(defn cols->rows
  [[[a1 b1 c1]
    [a2 b2 c2]
    [a3 b3 c3]]]
  [[a1 a2 a3]
   [b1 b2 b3]
   [c1 c2 c3]])

(defn parse-triangle-columns [input]
  (->> (parse-triangles input)
       (partition 3)
       (mapcat cols->rows)))

(defn possible-triangle? [sides]
  (let [[a b c] (sort sides)]
    (> (+ a b) c)))

(defn part-1 [input]
  (->> (parse-triangles input)
       (filter possible-triangle?)
       count))

(defn part-2 [input]
  (->> (parse-triangle-columns input)
       (filter possible-triangle?)
       count))

(assert (= (part-1 "5 10 25") 0))
(assert (= (parse-triangle-columns "101 301 501\n102 302 502\n103 303 503\n201 401 601\n202 402 602\n203 403 603")
           [[101 102 103] [301 302 303] [501 502 503] [201 202 203] [401 402 403] [601 602 603]]))

(let [input (slurp "day-03/input.txt")]
  (println "Part 1:" (part-1 input))
  (println "Part 2:" (part-2 input)))
