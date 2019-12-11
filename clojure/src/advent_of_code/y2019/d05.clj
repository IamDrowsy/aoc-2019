(ns advent-of-code.y2019.d05
  (:require [clojure.string :as str]
            [advent-of-code.util :refer [parse-long get-input check]]
            [advent-of-code.y2019.intcode :as i]
            [clojure.set :as set]
            [criterium.core :refer [quick-bench]]
            [clojure.core.async :refer [<!!]]))

(def day 5)

(defn solve-1 [input]
  (last (<!! (i/start (i/parse-intcode input) [1]))))

(defn solve-2 [input]
  (last (<!! (i/start (i/parse-intcode input) [5]))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))