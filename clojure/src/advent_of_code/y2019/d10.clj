(ns advent-of-code.y2019.d10
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [clojure.math.combinatorics :as combo]
    [neo4j-clj.core :as db]
    [clojure.string :as str]))

(def day 10)

(defn asteroid-locations [input]
  (persistent!
    (loop [x 0 y 0
           result (transient [])
           [c & r] input]
      (case c
        nil result
        \newline (recur 0 (inc y) result r)
        \. (recur (inc x) y result r)
        \# (recur (inc x) y (conj! result [x y]) r)))))

(defn normalized-line [[x1 y1] [x2 y2]]
  (if (= x1 x2)
    [x1 nil]
    (let [m (/ (- y2 y1) (- x2 x1))
          n (- y1 (* m x1))]
      [m n])))

(defn set-conj [s v1 v2]
  (if (nil? s) #{v1 v2}
               (conj s v1 v2)))

(defn build-lines [points]
  (reduce (fn [result [p1 p2]]
            (update result (normalized-line p1 p2)
                    set-conj p1 p2))
          {}
          (combo/combinations points 2)))

(defn ->label [x y]
  (str "PX" x "Y" y))

(defn points->nodes [points]
    (str/join ",\n" (map (fn [[x y]]
                           (str "(" (->label x y)  " {x: " x ", y: " y "})")) points)))

(defn points->relation [[[x1 y1] [x2 y2]]]
  (str "(" (->label x1 y1) ")-[:SEES]->(" (->label x2 y2) ")"))

(defn lines->relations [lines]
  (str/join ",\n" (mapcat #(map points->relation (partition 2 1 (sort %))) (vals lines))))

(defn input->query [input]
  (let [points (asteroid-locations input)
        lines (build-lines points)]
    (str "CREATE " (points->nodes points) ",\n" (lines->relations lines))))

(db/defquery all-nodes "MATCH (x) return x")
(db/defquery sees-count "MATCH (x {x: $x, y: $y})--(y) return count(y)")

(defn fill-db [db input]
  (db/execute (db/get-session db)
              (input->query input)))

(defmacro with-filled-db [input run-fun]
  `(let [db# (db/create-in-memory-connection)
         _# (fill-db db# ~input)
         result# (~run-fun db#)]
     ((:destroy-fn db#))
     result#))

(defn sees-most-asteriods [db]
  (with-open [session (db/get-session db)]
    (let [all-nodes (map :x (all-nodes session))
          best (last (sort-by #(first (vals (first (sees-count session %)))) all-nodes))]
      {:point best
       :number (first (vals (first (sees-count session best))))})))

(defn solve-1 [input]
  (:number (with-filled-db input sees-most-asteriods)))

(defn degree [[x1 y1] [x2 y2]]
  (- (Math/toDegrees (Math/atan2 (- x2 x1) (- y2 y1))) 180))

(defn solve-2 [input]
  (let [points (asteroid-locations input)
        {:keys [x y]} (:point (with-filled-db input sees-most-asteriods))
        best-point [x y]]
    (-> (->> points
             (group-by #(degree best-point %))
             sort
             reverse)
        (nth 199)
        second
        first
        (#(+ (* 100 (first %))
             (second %))))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))