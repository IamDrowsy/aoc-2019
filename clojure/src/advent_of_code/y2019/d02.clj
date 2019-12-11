(ns advent-of-code.y2019.d02
  (:require [clojure.string :as str]
            [advent-of-code.y2019.intcode :as i]
          [advent-of-code.util :refer [parse-long get-input check]]))

(def day 2)

(defn run-programm [intcode [noun verb]]
  {:result (-> intcode
               (i/set-absolute 1 noun)
               (i/set-absolute 2 verb)
               (i/run)
               (i/get-absolute 0))
   :verb verb
   :noun noun})

(defn solve-1 [input]
  (:result (run-programm (i/init-intcode [] input) [12 2])))

(defn solve-2 [input]
  (let [programm (i/init-intcode [] input)
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

