(ns advent-of-code.y2019.d01
  (:require [clojure.string :as str]
            [advent-of-code.util :refer [parse-long get-input]]
            [criterium.core :refer [quick-bench]]))

(defn fuel-requirement [weight]
  (- (quot weight 3) 2))

(defn solve-1 []
  (transduce (comp (map parse-long)
                   (map fuel-requirement))
             +
             (str/split-lines (get-input 1))))

(defn total-fuel-requirement [weight]
  (transduce (comp (drop 1)
                   (take-while pos-int?))
             +
             (iterate fuel-requirement weight)))

(defn solve-2 []
  (transduce (comp (map parse-long)
                   (map total-fuel-requirement))
             +
             (str/split-lines (get-input 1))))

(defn run []
  (assert (= 3361976 (solve-1)))
  (assert (= 5040085 (solve-2))))
