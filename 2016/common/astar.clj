(ns common.astar
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn- build-path [goal came-from]
  (loop [path [], node goal]
    (if-let [parent (came-from node)]
      (recur (conj path node) parent)
      (reverse path))))

(defn shortest-path
  "Find the shortest path between `start` and `goal`.

  - Uses `(heuristic a b))` to estimate the distance between nodes.
  - Uses `(get-neighbours a)` to find the neighbours of a given node."
  [start goal heuristic get-neighbours]
  (let [frontier (ref (priority-map start 0))
        came-from (ref {start nil})
        cost-so-far (ref {start 0})
        found-goal? (ref false)]
    (dosync
     (while (and (not-empty @frontier) (not @found-goal?))
       (let [[current] (peek @frontier)]
         (alter frontier pop)
         (when (= current goal)
           (ref-set found-goal? true))
         (doseq [next (get-neighbours current)]
           (let [new-cost (inc (@cost-so-far current))
                 next-cost (@cost-so-far next)]
             (when (or (nil? next-cost) (< new-cost next-cost))
               (alter frontier assoc next (heuristic next goal))
               (alter came-from assoc next current)
               (alter cost-so-far assoc next new-cost))))))
      (build-path goal @came-from))))
