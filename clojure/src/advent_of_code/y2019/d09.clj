(ns advent-of-code.y2019.d09
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [clojure.string :as str]
    [clojure.core.async :as a :refer [thread >!! <!! chan]]
    [advent-of-code.y2019.intcode :as i]))

(def day 9)

(defn solve-1 [input]
  (first (i/drain (i/start (i/parse-intcode input) [1]))))

(defn solve-2 [input]
  (first (i/drain (i/start (i/parse-intcode input) [2]))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))