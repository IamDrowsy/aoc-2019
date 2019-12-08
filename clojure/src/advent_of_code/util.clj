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