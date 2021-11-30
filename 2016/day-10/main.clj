(ns advent-of-code.day-10
  (:require [clojure.string :as str]
            [clojure.core.async :refer [<! <!! go put! chan]]))

(def chan-id-pattern #"(bot|output) (\d+)")
(def goes-rule-pattern #"value (\d+) goes to bot (\d+)")
(def gives-rule-pattern #"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)")

(defn- parse-chan-ids
  "Returns all channel ids in `s`"
  [s]
  (for [[_ type num] (re-seq chan-id-pattern s)]
    [(keyword type) (Integer/parseInt num)]))

(defn- parse-goes-rule
  "Returns the variable components of a `\"value N goes to bot M\"` rule."
  [s]
  (when-let [[_ val bot] (re-matches goes-rule-pattern s)]
    [(Integer/parseInt val) (Integer/parseInt bot)]))

(defn- parse-gives-rule
  "Returns the variable components of a `\"bot N gives low to A and high to B\"` rule."
  [s]
  (when-let [[_ val lt li ht hi] (re-matches gives-rule-pattern s)]
    [[:bot (Integer/parseInt val)]
     [(keyword lt) (Integer/parseInt li)]
     [(keyword ht) (Integer/parseInt hi)]]))

(defn- setup-factory
  "Parses bots/outputs from input and returns a factory with channels initialized."
  [input]
  (let [factory {:bot {} :output {} :log (chan)}
        chan-ids (parse-chan-ids input)
        setup-chan (fn [factory ks] (assoc-in factory ks (chan)))]
    (reduce setup-chan factory chan-ids)))

(defn- try-goes-rule!
  "Tries to evaluate a rule like `\"value N goes ...\"`."
  [factory s]
  (when-let [[val bot] (parse-goes-rule s)]
    (put! (get-in factory [:bot bot]) val)))

(defn- try-gives-rule!
  "Tries to evaluate a rule like `\"bot N gives...\"`.

   Each 'give' operation will be put onto the log channel."
  [factory s]
  (when-let [[bot lo hi] (parse-gives-rule s)]
    (go
      (let [chan-bot (get-in factory bot)
            chan-lo (get-in factory lo)
            chan-hi (get-in factory hi)
            chan-log (:log factory)
            a (<! chan-bot)
            b (<! chan-bot)]
        (put! chan-lo (min a b))
        (put! chan-hi (max a b))
        (put! chan-log [bot [a b]])))))

(defn- no-such-rule! [s]
  (throw (Exception. (str "No such rule: " s))))

(defn simulate-factory!
  "Creates a factory and applies a list of rules."
  [input]
  (let [factory (setup-factory input)]
    (doseq [s (str/split-lines input)]
      (or (try-goes-rule! factory s)
          (try-gives-rule! factory s)
          (no-such-rule! s)))
    factory))

(defn part-1
  "Simulates a factory with the rules in `input` then reads the log channel
  until finding a log from bot that compared the values in `xs`."
  [xs input]
  (let [factory (simulate-factory! input)
        logs (:log factory)]
    (loop [[bot ys] (<!! logs)]
      (if (= (set xs) (set ys))
        (second bot)
        (recur (<!! logs))))))

(defn part-2
  "Simulates a factory with the rules in `input` then multiplies the results
   from the output channels selected by `ids`."
  [ids input]
  (let [factory (simulate-factory! input)
        outputs (:output factory)
        chans (vals (select-keys outputs ids))
        values (map <!! chans)]
    (reduce * values)))

(comment
  (part-1 [5 2] "value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2")

  (part-2 [0 1 2] "value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2"))

(let [input (slurp "day-10/input.txt")]
  (println "Part 1:" (part-1 [61 17] input))
  (println "Part 2:" (part-2 [0 1 2] input)))

