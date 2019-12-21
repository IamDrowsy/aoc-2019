(ns advent-of-code.y2019.d21
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [clojure.string :as str]
    [clojure.core.async :as a :refer [thread >!! <!! chan]]
    [advent-of-code.y2019.intcode :as i]))

(def day 21)

(defn run-ascii [intcode input]
  (i/drain (i/start intcode (map int input))))

(def programm1
  (str/join "\n" ["NOT A T" "OR T J"
                  "NOT B T" "OR T J"
                  "NOT C T" "OR T J"
                  "AND D J"
                  "WALK" ""]))

(def programm2
  (str/join "\n" ["NOT A T" "OR T J"
                  "NOT B T" "OR T J"
                  "NOT C T" "OR T J"
                  "AND D J"
                  "NOT J T"
                  "OR E T"
                  "OR H T"
                  "AND T J"
                  "RUN" ""]))

(defn solve-1 [input]
  (last (run-ascii (i/parse-intcode input) programm1)))

(defn solve-2 [input]
  (last (run-ascii (i/parse-intcode input) programm2)))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))