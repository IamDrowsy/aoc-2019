(ns advent-of-code.util
  (:require [clj-http.client :as client]
            [clojure.string :as str])
  (:import (java.io File)))

(defn parse-long [s]
  (Long/parseLong s))

(defn- get-input-from-aoc [day]
  (let [session (slurp "../session")]
    (:body
      (client/get (format "https://adventofcode.com/2019/day/%s/input" day)
                  {:headers {"Cookie" (str "session=" session)}}))))

(defn- get-input* [day]
  (let [input-file (format "../data/d%02d-input.txt" day)]
    (if (.exists (File. input-file))
      (slurp input-file)
      (let [input (str/trim (get-input-from-aoc day))]
        (spit input-file input)
        input))))

(defn check [day part solution]
  (let [solution-file (format "../data/d%02d-part-%d.txt" day part)]
    (if (.exists (File. solution-file))
      (assert (= (slurp solution-file) (str solution)))
      (spit solution-file solution))))

(defn fixed-point [f x]
  "Calculates the fixed point of f with respect to x."
  (reduce #(if (= %1 %2) (reduced %1) %2)
          (iterate f x)))

(def get-input (memoize get-input*))

(defn draw-grid [char-mapping m]
  (let [min-x (first (sort (map first (keys m))))
        max-x (last (sort (map first (keys m))))
        min-y (first (sort (map second (keys m))))
        max-y (last (sort (map second (keys m))))]
    (->>
      (for [x (range max-x (dec min-x) -1)
            y (range min-y (inc max-y))]
        (or (char-mapping (m [x y])) " "))
      (partition (inc (- max-y min-y)))
      (mapv #(str/join %))
      (str/join "\n"))))

(defn digits [^long n]
  (if (< n 10)
    [n]
    (conj (digits (quot n 10)) (rem n 10))))