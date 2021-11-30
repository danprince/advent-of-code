(ns advent-of-code.day-11
  (:require [clojure.string :as str]
            [clojure.set :refer [union difference]]))

;; Not sure where I'm going wrong here. Tried uniform cost search and
;; got that to produce correct answers for the example data but not
;; the input data.
;;
;; Then tried a simpler BFS but can't quite get that right either.
;;
;; One to revisit...

(defn- all-possible-components [{:keys [floors]}]
  (let [elems (set (mapcat #(map :element %) floors))
        pairs (for [elem elems] [{:type :generator, :element elem}
                                 {:type :microchip, :element elem}])]
    (apply concat pairs)))

(defn- item->string [{type :type elem :element}]
  (str/upper-case
   (str (first elem) (second (str type)))))

(defn- items->string [{:keys [floors] :as state} idx]
  (let [items (nth floors idx)
        possible (all-possible-components state)
        parts (map #(if (contains? items %) (item->string %) ". ") possible)]
    (str/join " " parts)))

(defn- print-floor [{:keys [elevator] :as state} idx]
  (let [floor (inc idx)
        elev (if (= elevator idx) "E " ". ")
        items (items->string state idx)]
    (printf "F%d %s %s \n" floor elev items)))

(defn print-state [{:keys [floors] :as state}]
  (doseq [i (reverse (range (count floors)))]
    (print-floor state i)))

(defn parse-microchips [s]
  (for [[_ name] (re-seq #"([a-z]+)-compatible microchip" s)]
    {:type :microchip :element name}))

(defn parse-generators [s]
  (for [[_ name] (re-seq #"([a-z]+) generator" s)]
    {:type :generator :element name}))

(defn parse-line [s]
  (set (concat (parse-microchips s) (parse-generators s))))

(defn parse-state [input]
  {:elevator 0
   :floors (mapv parse-line (str/split-lines input))})

(defn valid-floor?
  "Returns `true` if there are no microchips will be irradiated, otherwise `false`."
  [items]
  (let [{microchips :microchip, generators :generator} (group-by :type items)
        microchips (map :element microchips)
        generators (set (map :element generators))]
    (if (empty? generators)
      ;; If there are no generators, chips can't be irradiated.
      true
      ;; Otherwise, make sure every chip has a generator
      (every? #(contains? generators %) microchips))))

(let [LM {:type :microchip, :element "lithium"}
      LG {:type :generator, :element "lithium"}
      HM {:type :microchip, :element "hydrogen"}
      HG {:type :generator, :element "hydrogen"}]
  (assert (valid-floor? #{LM}) "one chip")
  (assert (valid-floor? #{LG}) "one generator")
  (assert (valid-floor? #{LM HM}) "two chips")
  (assert (valid-floor? #{LG HG}) "two generators")
  (assert (valid-floor? #{LM LG}) "matching pair")
  (assert (valid-floor? #{LM LG HM HG}) "two matching pairs")
  (assert (not (valid-floor? #{LM HG})) "mismatched pair")
  (assert (not (valid-floor? #{LM HG HM})) "matched pair with extra chip"))

(defn valid-state?
  "Returns true if all floors are valid (no irradiation)."
  [{:keys [floors]}] (every? valid-floor? floors))

(defn final-state?
  "Returns true if all items are on the top floor."
  [{:keys [floors]}]
    (every? empty? (butlast floors)))

(defn combinations
  "Returns all unique combinations of up to 2 items."
  [items] (set (for [x items, y items] (set [x y]))))

(defn move-items
  "Returns the new state after moving items from the `src` floor to the `dst`
  floor."
  [{src :elevator :keys [floors] :as state} items dst]
  {:pre [(not= dst src) ;; can't move lift to the floor it's already on
         (< -1 dst (count floors)) ;; must move to a valid floor
         (<= (count items) 2)]} ;; can't move more than 2 items
  (-> state
      (assoc :elevator dst)
      (update-in [:floors src] #(difference % items))
      (update-in [:floors dst] #(union % items))))

(defn all-adjacent-states
  "Returns a set of all valid adjacent states."
  [{:keys [floors elevator] :as state}]
  (let [items (nth floors elevator)
        groups (combinations items)
        above (inc elevator)
        below (dec elevator)]
    (concat
     (when (< above (count floors))
       (map #(move-items state % above) groups))
     (when (>= below 0)
       (map #(move-items state % below) groups)))))

(defn adjacent-states [state]
  (->> state all-adjacent-states (filter valid-state?) set))

(defn dist [{:keys [floors]}]
  (let [n (count floors)]
    (reduce + (for [i (range n)]
                (* (- n i 1) (count (nth floors i)))))))

(def dist-memo (memoize dist))

(defn simplify-floor [items]
  (map :type items))

(defn simplify-state [{:keys [floors] :as state}]
  (assoc state :floors (map simplify-floor floors)))

(def simplify (memoize simplify-state))

(defn find-path [route end]
  (loop [path []
         node end]
    (if (nil? node)
      (reverse path)
      (recur (conj path node) (get route node)))))

(defn breadth-first-search [start]
  (loop [queue [start]
         visited #{}
         sequence {}]
    (when-let [[state & queue] queue]
      (if (final-state? state)
        (find-path sequence state)
        (let [visited (conj visited (simplify state))
              neighbours (adjacent-states state)
              neighbours (remove #(contains? visited (simplify %)) neighbours)
              sequence (into sequence (map vector neighbours (repeat state)))
              queue (concat neighbours queue)]
          (recur queue visited sequence))))))

(defn gen [elem] {:type :generator, :element elem})
(defn mcr [elem] {:type :microchip, :element elem})

(defn part-1 [input]
  (let [start (parse-state input)
        [_start & path] (breadth-first-search start)]
    (count path)))

(defn part-2 [input] nil)

(let [input (slurp "day-11/input.txt")]
  (println "Part 1:" (part-1 input))
  (println "Part 2:" (part-2 input)))
