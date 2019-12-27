(ns advent-of-code.y2019.d25
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [clojure.string :as str]
    [clojure.core.async :as a :refer [thread >!! <!! chan]]
    [advent-of-code.y2019.intcode :as i]))

(def day 25)

(defn printer [started-intcode]
  (thread
    (loop [msg []]
      (let [out (<!! (i/get-outchan started-intcode))]
        (if (nil? out)
          (println (apply str (map char msg)))
          (if (= out 63)
            (do (println (apply str (map char msg)))
                (recur []))
            (recur (conj msg out))))))))

(defn send-command [i command]
  (a/onto-chan (i/get-inchan i) (map int (str command "\n")) false))

(def items ["shell" "space heater" "jam" "astronaut ice cream" "space law space brochure"
            "asterisk" "klein bottle" "spool of cat6"])

(defn r [i commands]
  (doseq [c commands]
    (send-command i c)
    (Thread/sleep 50)))

(def commands
  ["south" "east" "take space heater"
   "west" "north" "west" "north" "north" "take astronaut ice cream"
   "south" "east" "south" "take asterisk"
   "south" "take klein bottle"
   "north" "north" "west" "south" "south" "west" "south"])

(defn drop-all [i]
  (r i (mapv #(str "drop " %) items)))

(defn try-run [i items]
  (drop-all i)
  (r i (mapv #(str "take " %) items))
  (r i ["south"]))

(defn solve-1 [input]
  (let [i (i/start (i/parse-intcode input))]
    (printer i)
    (r i commands)))

(defn run []
  (check day 1 (solve-1 (get-input day))))