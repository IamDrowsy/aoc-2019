(ns advent-of-code.y2019.d01
  (:require [clojure.string :as str]
            [advent-of-code.util :refer [parse-long get-input check]]
            [criterium.core :refer [quick-bench]]))

(def day 1)

(defn fuel-requirement [weight]
  (- (quot weight 3) 2))

(defn solve-1 [input]
  (transduce (comp (map parse-long)
                   (map fuel-requirement))
             +
             (str/split-lines (get-input 1))))

(defn total-fuel-requirement [weight]
  (transduce (comp (drop 1)
                   (take-while pos-int?))
             +
             (iterate fuel-requirement weight)))

(defn solve-2 [input]
  (transduce (comp (map parse-long)
                   (map total-fuel-requirement))
             +
             (str/split-lines input)))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))
