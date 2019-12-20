(ns advent-of-code.y2019.d19
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [medley.core :refer [find-first]]
    [advent-of-code.y2019.intcode :as i]
    [advent-of-code.grid :as g]
    [advent-of-code.util :as u]))

(def day 19)

(defn guess [x y row]
  (long (* row (/ x y))))

(defn beam? [intcode x y]
  (first (i/drain (i/start intcode [x y]))))

(defn find-last-one [beam-fn start-x step]
  (- (find-first #(zero? (beam-fn %))
                 (range start-x (+ start-x (* step start-x)) step))
     step))

(defn find-first-one [beamfn start-x step]
  (find-first #(= 1 (beamfn %))
              (range start-x (+ start-x (* step start-x)) step)))

(defn find-borders [intcode {:keys [left right y]} row]
  (let [beamfn #(beam? intcode % row)]
    (let [guess-left (guess left y row)
          guess-right (guess right y row)]
      {:left (if (zero? (beamfn guess-left))
               (find-first-one beamfn guess-left 1)
               (find-last-one beamfn guess-left -1))
       :right (if (zero? (beamfn guess-right))
                (find-first-one beamfn guess-right -1)
                (find-last-one beamfn guess-right 1))
       :y row})))

(defn solve-1 [input]
  (let [i (i/parse-intcode input)
        high-guess (reduce (fn [guess row] (find-borders i guess row))
                           {:left 10 :right 30 :y 20} [20 200 1000])]
    (->> (range 4 50)
         (map #(find-borders i high-guess %))
         (reduce (fn [sum {:keys [left right]}]
                   (+ sum 1 (- right left)))
                 1))))

(defn square-size [intcode high-guess row]
  (inc (- (:right (find-borders intcode high-guess row))
          (:left (find-borders intcode high-guess (+ row 99))))))

(defn result [intcode high-guess row]
  (+ (* 10000 (:left (find-borders intcode high-guess (+ row 99))))
     row))

(defn solve-2 [input]
  (let [intcode (i/parse-intcode input)
        high-guess (reduce (fn [guess row] (find-borders intcode guess row))
                           {:left 10 :right 30 :y 20} [20 200 1000 10000])]
    (result intcode high-guess (u/binary-search #(square-size intcode high-guess %) #(<= 100 %) 20))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))