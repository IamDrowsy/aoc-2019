(ns advent-of-code.y2019.d02
  (:require [clojure.string :as str]
          [advent-of-code.util :refer [parse-long get-input check]]))

(def day 2)

(defn parse-programm [input]
  (assoc (zipmap (range) (mapv parse-long (str/split input #",")))
    :current 0))

(defn step [programm]
  (let [current (:current programm)
        op-code (programm current)]
    (case op-code
      1 (assoc programm (programm (+ current 3))
                        (+ (programm (programm (+ current 1)))
                           (programm (programm (+ current 2))))
                        :current (+ current 4))
      2 (assoc programm (programm (+ current 3))
                        (* (programm (programm (+ current 1)))
                           (programm (programm (+ current 2))))
                        :current (+ current 4))
      99 (assoc programm
           :current :done
           :result (programm 0)))))

(defn run-programm [programm [noun verb]]
  (->> programm
     (#(assoc % 1 noun 2 verb :noun noun :verb verb))
     (iterate step)
     (drop-while #(not= (:current %) :done))
     (first)))

(defn solve-1 [input]
  (:result (run-programm (parse-programm input) [12 2])))

(defn solve-2 [input]
  (let [programm (parse-programm input)
        result (->> (for [noun (range 100)
                          verb (range 100)]
                      [noun verb])
                    (pmap (partial run-programm programm))
                    (drop-while #(not= (:result %) 19690720))
                    (first))]
    (+ (* 100 (:noun result))
       (:verb result))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))

