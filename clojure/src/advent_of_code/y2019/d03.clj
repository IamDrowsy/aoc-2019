(ns advent-of-code.y2019.d03
  (:require [clojure.string :as str]
            [advent-of-code.util :refer [parse-long get-input check]]
            [clojure.set :as set]))

(def day 3)

(defn parse-dir-string [dir-string]
  (re-find #"(\w)(\d+)" dir-string))

(defn extend-wire [wire dir-string]
  (let [[_ dir dist-string] (re-find #"(\w)(\d+)" dir-string)
        dist (parse-long dist-string)
        [curr-x curr-y] (first wire)]
    (reduce conj wire (case dir
                        "R" (map #(vector % curr-y) (range curr-x (+ curr-x dist)))
                        "L" (map #(vector % curr-y) (range curr-x (- curr-x dist) -1))
                        "U" (map #(vector curr-x %) (range curr-y (+ curr-y dist)))
                        "D" (map #(vector curr-x %) (range curr-y (- curr-y dist) -1))))))

(defn wire-points [wire-string]
  (into #{} (reduce extend-wire '([0 0]) (str/split wire-string #","))))

(defn dist-to-zero [[^long x ^long y]]
  (+ (Math/abs x) (Math/abs y)))

(defn solve-1 [input]
  (dist-to-zero (second (sort-by dist-to-zero (apply set/intersection (map wire-points (str/split-lines input)))))))

(defn solve-2 [input])


(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))

