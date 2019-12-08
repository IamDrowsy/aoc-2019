(ns advent-of-code.y2019.d05
  (:require [clojure.string :as str]
            [advent-of-code.util :refer [parse-long get-input check]]
            [clojure.set :as set]
            [criterium.core :refer [quick-bench]]))

(def day 5)

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(defn digits [^long n]
  (if (< n 10)
    [n]
    (conj (digits (quot n 10)) (rem n 10))))

(defn fast-digit-diffs [digits]
  (loop [result []
         last nil
         [f & r] digits]
    (cond (nil? f) result
          (nil? last) (recur result f r)
          :else (recur (conj result (- ^long f ^long last))
                       f
                       r))))

(defn non-decreasing-with-consecutive [n]
  (zero? ^long (first (sort (fast-digit-diffs (digits n))))))

(defn solve-1 [input]
  (let [[start end] (str/split input #"-")]
    (count (filter non-decreasing-with-consecutive (range (parse-long start) (parse-long end))))))

(defn has-excatly-two-consecutive-digits [digits]
  ((->> digits
        (partition-by identity)
        (group-by count))
   2))

(defn non-decreasing-with-single-consecutive [n]
  (let [d (digits n)
        dds (fast-digit-diffs d)]
    (and (zero? ^long (first (sort dds)))
         (has-excatly-two-consecutive-digits d))))

(defn solve-2 [input]
  (let [[start end] (str/split input #"-")]
    (count (filter non-decreasing-with-single-consecutive (range (parse-long start) (parse-long end))))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))

