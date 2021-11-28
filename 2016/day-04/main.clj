(ns advent-of-code.day-04
  (:require [clojure.string :as str]))

(defn parse-room [s]
  (let [[_ name sector checksum] (re-matches #"(.*)-(\d+)\[(.*)\]" s)]
    {:name name
     :sector (Integer/parseInt sector)
     :checksum checksum}))

(defn compare-checksum [[c1 f1] [c2 f2]]
  (if (= f1 f2)
    (compare c1 c2)
    (compare f2 f1)))

(defn checksum [room]
  (->> (str/replace (:name room) #"-" "")
       frequencies
       (sort compare-checksum)
       (map first)
       (take 5)
       (apply str)))

(defn real-room? [room]
  (= (:checksum room) (checksum room)))

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn shift-char [ch n]
  (let [i (str/index-of alphabet ch)
        i (+ i n)
        i (mod i (count alphabet))]
    (nth alphabet i)))

(defn shift-cipher [str n]
  (str/join (for [ch str]
              (case ch
                \- \space
                (shift-char ch n)))))

(defn decrypt-name [{:keys [name sector]}]
  (shift-cipher name sector))

(defn is-north-pole-storage [room]
  (= (decrypt-name room) "northpole object storage"))

(defn part-1 [input]
  (->> (str/split-lines input)
       (map parse-room)
       (filter real-room?)
       (map :sector)
       (reduce +)))

(defn part-2 [input]
  (->> (str/split-lines input)
       (map parse-room)
       (filter is-north-pole-storage)
       first
       :sector))

(assert (= (part-1 "aaaaa-bbb-z-y-x-123[abxyz]") 123))
(assert (= (part-1 "a-b-c-d-e-f-g-h-987[abcde]") 987))
(assert (= (part-1 "not-a-real-room-404[oarel]") 404))
(assert (= (part-1 "totally-real-room-200[decoy]") 0))

(assert (= (shift-cipher "qzmt-zixmtkozy-ivhz" 343) "very encrypted name"))

(let [input (slurp "day-04/input.txt")]
  (println "Part 1:" (part-1 input))
  (println "Part 2:" (part-2 input)))
