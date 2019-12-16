(ns advent-of-code.y2019.d16
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [clojure.string :as str]
    [medley.core :refer [join]]
    [clojure.core.matrix :as m]))

(def day 16)

(m/set-current-implementation :vectorz)

(defn parse-input [input-string]
  (m/array (mapv #(parse-long (str %)) input-string)))

(defn pattern [length pos]
  (into [] (take length (drop 1 (cycle (concat (repeat pos 0) (repeat pos 1) (repeat pos 0) (repeat pos -1)))))))

(defn phase-matrix [length]
  (m/sparse-matrix (mapv (partial pattern length) (range 1 (inc length)))))

(defn phase [phase-matrix input]
  (m/emap! #(rem % 10.0) (m/abs! (m/mmul phase-matrix input))))

(defn solve-1 [input-string]
  (let [input (parse-input input-string)
        pm (phase-matrix (m/ecount input))]
    (->> (nth (iterate (partial phase pm) input) 100)
         (take 8)
         (mapv long)
         (str/join))))

(defn reversed-full-input [input]
  (apply m/join (repeat 10000 (parse-input (reverse input)))))

(defn offset [input]
  (parse-long (subs input 0 7)))

(defn relevant-vec [input]
  (let [rfv (reversed-full-input input)]
    (m/subvector rfv 0 (- (m/ecount rfv) (offset input)))))

(defn phase-2 [vec]
  (map #(mod % 10) (reductions + vec)))

(defn solve-2 [input]
  (let [v (relevant-vec input)]
    (str/join (map long (reverse (take-last 8 (nth (iterate phase-2 v) 100)))))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))