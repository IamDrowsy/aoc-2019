(ns advent-of-code.y2019.d17
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [clojure.string :as str]
    [clojure.core.async :as a :refer [thread >!! <!! chan]]
    [advent-of-code.y2019.intcode :as i]))

(def day 17)

(defn parse-field [input]
  (let [camera (i/drain (i/start (i/parse-intcode input) []))
        width (count (first (partition-by #(= 10 %) camera)))]
    {:field (->> camera
                 (remove #(= 10 %))
                 (into []))
     :width width}))

(defn neighbors [{:keys [field width]} index]
  (map field [(inc index) (dec index) (+ index width) (- index width)]))

(defn solve-1 [input]
  (partition-by #(= 10) (i/drain (i/start (i/parse-intcode input) [1]))))

(defn solve-2 [input]
  (first (i/drain (i/start (i/parse-intcode input) [2]))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))