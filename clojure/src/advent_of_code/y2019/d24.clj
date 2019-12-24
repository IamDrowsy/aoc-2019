(ns advent-of-code.y2019.d24
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [cuerdas.core :as str]
    [medley.core :as m]
    [clojure.set :as set]))

(def day 24)
; Part 1 with bit operations

(defn parse-input [input]
  (-> input
      (str/replace "\n" "")
      (str/replace "#" "1")
      (str/replace "." "0")
      (str/reverse)
      (Long/parseLong 2)))

(defn neighbor-numbers [number]
  [(bit-shift-right number 5)
   (bit-and (bit-shift-left number 5) 2r000001111111111111111111111111)
   (bit-and (bit-shift-left number 1) 2r01111011110111101111011110)
   (bit-and (bit-shift-right number 1) 2r0111101111011110111101111)])

(def a bit-and)
(def o bit-or)
(def n bit-not)

(defn step [x]
  (let [neighbors (neighbor-numbers x)
        [e f g h] neighbors
        [ne nf ng nh] (map bit-not neighbors)
        nx (bit-not x)]
    (o (a e nf ng nh)
       (a ne f ng nh)
       (a ne nf g nh)
       (a ne nf ng h)
       (a nx ne nf g h)
       (a nx ne f ng h)
       (a nx e nf ng h)
       (a nx ne f g nh)
       (a nx e nf g nh)
       (a nx e f ng nh))))

(defn find-first-repetation [seq]
  (reduce (fn [known e]
            (if (known e)
              (reduced e)
              (conj known e)))
          #{}
          seq))

(defn solve-1 [input]
  (find-first-repetation (iterate step (parse-input input))))

; Part 2 with set of 3-dim coords

(defn parse-grid [input]
  (set (keys (m/filter-vals (fn [v] (re-matches #"#" (str v)))
                            (apply merge (map #(into {} %)
                                              (map-indexed (fn [y line]
                                                             (map-indexed (fn [x cell]
                                                                            [[x y 0] cell])
                                                                          line))
                                                           (str/split input))))))))
(defn up [[x y z]]
  (cond
    (= [x y] [2 3]) #_> (mapv (fn [x] [x 4 (dec z)]) (range 5))
    (= y 0) #_> [[2 1 (inc z)]]
    :else  #_> [[x (dec y) z]]))

(defn down [[x y z]]
  (cond
    (= [x y] [2 1]) #_> (mapv (fn [x] [x 0 (dec z)]) (range 5))
    (= y 4) #_> [[2 3 (inc z)]]
    :else #_> [[x (inc y) z]]))

(defn left [[x y z]]
  (cond
    (= [x y] [3 2]) #_> (mapv (fn [y] [4 y (dec z)]) (range 5))
    (= x 0) #_> [[1 2 (inc z)]]
    :else #_> [[(dec x) y z]]))

(defn right [[x y z]]
  (cond
    (= [x y] [1 2]) #_> (mapv (fn [y] [0 y (dec z)]) (range 5))
    (= x 4) #_> [[3 2 (inc z)]]
    :else #_> [[(inc x) y z]]))

(defn neighbors* [v]
  (set (m/join ((juxt up down left right) v))))

(def neighbors (memoize neighbors*))

(defn neighbor# [prev-alive v]
  (count (set/intersection (neighbors v) prev-alive)))

(defn alive? [prev-alive v]
  (let [n (neighbor# prev-alive v)]
    (or (= 1 n)
        (and (= 2 n) (not (prev-alive v))))))

(defn step2 [grid]
  (let [to-check (into grid (m/join (map neighbors grid)))]
    (into #{} (filter (partial alive? grid))
          to-check)))

(defn solve-2 [input]
  (count (first (drop 200 (iterate step2 (parse-grid input))))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))