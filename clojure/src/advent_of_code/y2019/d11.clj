(ns advent-of-code.y2019.d11
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [clojure.string :as str]
    [clojure.core.async :as a :refer [thread >!! <!! chan]]
    [advent-of-code.y2019.intcode :as i]))

(def day 11)

(defn new-direction [old-direction turn-instruction]
  (case turn-instruction
    0 (mod (inc old-direction) 4)
    1 (mod (dec old-direction) 4)))

(defn new-pos [[old-x old-y] direction]
  (case direction
    0 [(inc old-x) old-y] 1 [old-x (inc old-y)]
    2 [(dec old-x) old-y] 3 [old-x (dec old-y)]))

(defn start-robot [in-chan out-chan initial]
  (thread
    (loop [grid {[0 0] initial}
           pos [0 0]
           direction 0]
      #_(println "sending" (grid pos 0))
      (>!! out-chan (grid pos 0))
      (let [paint-instruction (<!! in-chan)
            turn-instruction (<!! in-chan)]
        #_(println "got instructions" paint-instruction turn-instruction)
        #_(println "grid" grid)
        (if paint-instruction
          (let [new-direction (new-direction direction turn-instruction)]
            (recur (assoc grid pos paint-instruction)
                   (new-pos pos new-direction)
                   new-direction))
          grid)))))

(defn paint-field [input initial]
  (let [intcode (i/parse-intcode input)
        camera (chan 20) instructions (chan 20)]
    (i/start intcode camera instructions)
    (<!! (start-robot instructions camera initial))))

(defn solve-1 [input]
  (count (paint-field input 0)))

(defn solve-2 [input]
  (paint-field input 1))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))