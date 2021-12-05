(ns day-15.main
  (:require [clojure.string :as str]))

(def disc-pattern #"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)\.")

(defn parse-disc [s]
  (if-let [[_ id n p] (re-matches disc-pattern s)]
    {:id (Integer/parseInt id)
     :positions (Integer/parseInt n)
     :offset (Integer/parseInt p)}
    (throw (Exception. (format "Could not parse disc: \"%s\"" s)))))

(defn parse-discs [input]
  (->> (str/split-lines input) (map parse-disc)))

(defn disc-open?
  "Returns true if the disc is open at time=t."
  [{:keys [offset positions]} t]
  (zero? (mod (+ offset t) positions)))

(defn every-disc-open?
  "Returns true if every disc will be open at the correct intervals,
  starting from time=t."
  [discs t]
  (every? #(disc-open? % (+ t (:id %))) discs))

(defn find-drop-time
  "Finds a time when it will be possible to drop a capsule through all discs."
  [discs]
  (loop [t 0]
    (if (every-disc-open? discs t)
      t
      (recur (inc t)))))

(defn part-1 [input]
  (let [discs (parse-discs input)]
    (find-drop-time discs)))

(defn part-2 [input]
  (let [discs (parse-discs input)
        new-disc {:id (inc (count discs)), :positions 11, :offset 0}
        discs (conj discs new-disc)]
    (find-drop-time discs)))

(let [example (slurp "day-15/example.txt")]
  (assert (= (part-1 example) 5)))

(let [input (slurp "day-15/input.txt")]
  (println "Part 1:" (part-1 input))
  (println "Part 2:" (part-2 input)))
