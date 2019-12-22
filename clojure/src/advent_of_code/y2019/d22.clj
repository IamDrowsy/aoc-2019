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

;a*i+b
(defn comp-step [cards# [a b] [a' b']]
  [(mod (* a a') cards#) (mod (+ (* a' b) b') cards#)])

(defn parse-line [card# reverse? line]
  (cond (str/starts-with? line "deal into")
        [-1 -1]
        (str/starts-with? line "deal with")
        (if reverse?
          [(bigint (u/modinv (last-number line) card#)) 0]
          [(last-number line) 0])
        (str/starts-with? line "cut")
        (if reverse?
          [1 (last-number line)]
          [1 (- (last-number line))])))

(defn apply-step [cards# [a b] index]
  (mod (+ (* a index) b) cards#))

(defn parse-input [^long card# reverse? input]
  (let [reverse-fn (if reverse? reverse identity)]
    (->> (str/split-lines input)
         (mapv (partial parse-line card# reverse?))
         reverse-fn
         (reduce (partial comp-step card#)))))

(defn solve-1 [input]
  (apply-step 10007 (parse-input 10007 false input) 2019))

(defn solve-2 [input]
  (let [cards# 119315717514047N
        step (parse-input cards# true input)]
    (long (apply-step cards# (u/fast-pow (partial comp-step cards#)
                                         step
                                         101741582076661)
                      2020N))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))