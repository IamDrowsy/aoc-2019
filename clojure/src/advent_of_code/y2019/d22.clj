(ns advent-of-code.y2019.d22
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [clojure.string :as str]
    [advent-of-code.util :as u]
    [fastmath.core :as fm]))

(def day 22)

(defn last-number [s]
  (parse-long (last(str/split s #"\s"))))

(defn deal-into-new [cards# index]
  (mod (- (inc index)) cards#))

(defn cut [cards# cut# index]
  (mod (+ index cut#) cards#))

(defn deal-with-inc [cards# inc# index]
  (mod (* index inc#) cards#))

(defn parse-line [card# line]
  (cond (str/starts-with? line "deal into")
        (partial deal-into-new card#)
        (str/starts-with? line "deal with")
        (partial deal-with-inc card# (last-number line))
        (str/starts-with? line "cut")
        (partial cut card# (- (last-number line)))))

(defn parse-input [^long card# input]
  (reduce comp (mapv (partial parse-line card#) (reverse (str/split-lines input)))))

(defn solve-1 [input]
  ((parse-input 10007 input) 2019))

(defn step [])

(defn solve-2 [input]
  (let [shuffle-fn (parse-input 119315717514047 input)
        step (shuffle-fn 2020)]
    (u/first-repetition shuffle-fn 2020)))


(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))