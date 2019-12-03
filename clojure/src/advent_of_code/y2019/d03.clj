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
                        "R" (map #(vector % curr-y) (range (inc curr-x) (inc (+ curr-x dist))))
                        "L" (map #(vector % curr-y) (range (dec curr-x) (dec (- curr-x dist)) -1))
                        "U" (map #(vector curr-x %) (range (inc curr-y) (inc (+ curr-y dist))))
                        "D" (map #(vector curr-x %) (range (dec curr-y) (dec (- curr-y dist)) -1))))))

(defn wire-points [wire-string]
  (reduce extend-wire '([0 0]) (str/split wire-string #",")))

(defn dist-to-zero [[^long x ^long y]]
  (+ (Math/abs x) (Math/abs y)))

(defn key-set [m]
  (set (keys m)))

(defn solve-1 [input]
  (dist-to-zero (second (sort-by dist-to-zero (apply set/intersection (map #(set (wire-points %)) (str/split-lines input)))))))

(defn wire-map [wire-string]
  (let [wp (wire-points wire-string)]
    (zipmap wp (range (dec (count wp)) -1 -1))))

(defn solve-2 [input]
  (let [wire-strings (str/split-lines input)
        [wire1 wire2] (mapv wire-map wire-strings)
        dist-sum (fn [point] (+ (wire1 point) (wire2 point)))
        nearest-point (second (sort-by dist-sum (set/intersection (key-set wire1) (key-set wire2))))]
    (dist-sum nearest-point)))


(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))

