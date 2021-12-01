(ns day-11
  (:require [clojure.string :as str]
            [clojure.set :refer [union difference]]
            [common.astar :refer [shortest-path]]))

(defn microchip [element] {:type :microchip :element element})
(defn generator [element] {:type :generator :element element})

(defn- all-possible-components
  "Generates a set of all possible components for a state. "
  [{:keys [floors]}]
  (let [elems (set (mapcat #(map :element %) floors))
        pairs (for [elem elems] [(generator elem) (microchip elem)])]
    (apply concat pairs)))

(defn- component->string
  "Returns the string representation for a component."
  [{type :type elem :element}]
  (str/upper-case
   (str (first elem) (second (str type)))))

(defn- floor->string
  "Returns the string representation of the floor at `idx`."
  [{:keys [floors] :as state} idx]
  (let [items (nth floors idx)
        possible (all-possible-components state)
        parts (map #(if (contains? items %) (component->string %) ". ") possible)]
    (str/join " " parts)))

(defn print-state [{:keys [floors elevator] :as state}]
  (doseq [idx (reverse (range (count floors)))]
    (let [floor (inc idx)
          elev (if (= elevator idx) "E " ". ")
          items (floor->string state idx)]
      (printf "F%d %s %s \n" floor elev items)))
  (println))

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
        m-elems (map :element microchips)
        g-elems (set (map :element generators))]
    (if (empty? g-elems)
      ;; If there are no generators, chips can't be irradiated.
      true
      ;; Otherwise, make sure every chip has a generator
      (every? #(contains? g-elems %) m-elems))))

(let [LM (microchip "lithium")
      HM (microchip "hydrogen")
      LG (generator "lithium")
      HG (generator "hydrogen")]
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
  "Returns a set of all adjacent states."
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
  (->> state all-adjacent-states (filter valid-state?)))

(defn dist [{:keys [floors]}]
  (let [top (dec (count floors))]
    (->>
     (map count floors)
     (map-indexed vector)
     (reduce (fn [score [floor count]]
               (+ score (* (- top floor) count))) 0))))

(def dist-memo (memoize dist))

(defn create-goal-state [{:keys [floors]}]
  (let [components (apply union floors)
        height (count floors)
        top (dec height)]
    {:elevator top
     :floors (vec (for [i (range height)]
                    (if (= i top) components #{})))}))

(defn part-1 [input]
  (let [start (parse-state input)
        goal (create-goal-state start)
        heuristic (fn [state _] (dist-memo state))
        path (shortest-path start goal heuristic adjacent-states)]
    (print-state start)
    (doseq [state path]
      (print-state state))
    (count path)))

(defn part-2 [_input] nil)

(comment
  (part-1 "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant."))

(let [input (slurp "day-11/input.txt")]
  (println "Part 1:" (part-1 input))
  (println "Part 2:" (part-2 input)))
