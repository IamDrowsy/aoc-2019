(ns advent-of-code.util
  (:require [clj-http.client :as client]
            [clojure.string :as str]
            [clojure.core.async :as a]
            [medley.core :as m])
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
      (let [input (str/trim-newline (get-input-from-aoc day))]
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

(defn bits [number]
  (->> number
       (iterate #(bit-shift-right % 1))
       (take-while pos?)
       (reverse)
       (map #(bit-and 0x01 %))))

(defn- powed-bases [max-exp pow-fn base]
  (loop [result [base]
         current-exp 1]
    (if (= current-exp max-exp)
      result
      (recur (conj result (pow-fn (last result) (last result)))
             (inc current-exp)))))

(defn fast-pow
  "Pow using binary notation of the expontent. with pow-fn a^2 = (pow-fn a a)"
  [pow-fn base exponent]
  (let [bits (vec (bits exponent))
        max-exp (count bits)
        bases (vec (reverse (powed-bases max-exp pow-fn base)))]
    (reduce (fn [result i]
              (if (zero? (bits i))
                result
                (pow-fn result (bases i))))
            (first bases)
            (range 1 max-exp))))


(defn between [i1 i2]
  (+ i1 (quot (- i2 i1) 2)))

(defn- binary-search* [f target-fn highest-false-index lowest-true-index this-index]
  (loop [hfi highest-false-index lti lowest-true-index  i this-index]
    (let [result (target-fn (f i))]
      (cond
        ;linear search case
        (and lti hfi (> 10 (- lti hfi)))
        (first (drop-while #(not (target-fn (f %))) (range hfi (inc lti))))

        (and (not result) (nil? lti))
        (recur i lti (* 2 i))

        (and result (nil? hfi))
        (recur hfi i (quot i 2))

        (not result)
        (recur i lti (between i lti))

        result
        (recur hfi i (between hfi i))))))

(defn binary-search
  "Finds the first index for that (target-fn (f index)) is true. Binary searches with the index"
  ([f target-fn]
   (binary-search f target-fn 1))
  ([f target-fn start-index]
   (binary-search* f target-fn nil nil start-index)))

(defn watch-channel [out-chan]
  (a/thread
    (loop [val (a/<!! out-chan)]
      (when val
        (println val) (recur (a/<!! out-chan))))))

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

(defn extgcd [a b]
  (if (zero? b)
    [a 1 0]
    (let [[g u v] (extgcd b (mod a b))]
      [g v (- u (* (quot a b) v))])))

(defn modinv [a m]
  (mod (second (extgcd a m)) m))

(defn iterate-indexed
  "Like iterate but f takes the index and the previous value"
  [f x]
  (map second (iterate (fn [[i x]]
                         [(inc i) (f i x)])
                       [0 x])))
