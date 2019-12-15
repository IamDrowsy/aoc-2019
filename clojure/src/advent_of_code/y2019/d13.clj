(ns advent-of-code.y2019.d13
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [clojure.string :as str]
    [net.cgrand.xforms :as x]
    [clojure.core.async :as a :refer [thread >!! <!! chan]]
    [advent-of-code.y2019.intcode :as i]
    [advent-of-code.util :as u]))

(def day 13)

(def tiles
  {0 " " 1 "|" 2 "#" 3 "-" 4 "O"})

(defn fill-board [board [x y id]]
  (assoc board [x y] (tiles id)))

(def count-blocks
  (comp (x/partition 3)
        (filter #(= 2 (last %)))
        x/count))

(defn solve-1 [input]
  (let [out-chan (chan 20)]
    (i/start (i/parse-intcode input) [] out-chan)
    (<!! (a/transduce count-blocks + 0 out-chan))))

(defn next-bx [game]
  (let [next-out (chan 20)]
    (i/resume @(:intcode game) [0] next-out)
    (first (i/drain-until-closed (a/transduce (comp (x/partition 3)
                                                    (filter (fn [[_ _ i]] (= i 4)))
                                                    (map first))
                                              + 0 next-out)))))

(defn init-game [intcode]
  {:px 0 :py 0
   :bx 0 :by 0
   :last-bx 0 :last-by 0
   :score nil
   :intcode intcode
   :blocks #{}
   ;this might be dynamic, so we usally would have to get this from the input
   :walls #{}})

(defn needed-input [{:keys [px] :as game}]
  (let [next-x (next-bx game)]
    #_(println last-bx bx (:by game) px (:py game) next-x)
    (cond (< px next-x) 1
          (< next-x px) -1
          :else 0)))

(defn input! [g val]
  (i/input! (:intcode g) val))

(defn process-output
  ([game]
   (:score game))
  ([game [x y i]]
   (cond
     (and (= -1 x) (= 0 y)) (do (println (count (:blocks game)))
                                (println i)
                                (assoc game :score i))
     (= i 3) (assoc game :px x :py y)
     (= i 4) (do (println (:bx game) (:by game))
               (input! game (needed-input (assoc game :bx x :by y)))
               (assoc game :bx x :by y))
     (= i 2) (update game :blocks conj [x y])
     (= i 0) (if (contains? (:blocks game) [x y])
               (update game :blocks disj [x y])
               game)
     :else game)))

(defn solve-2 [input]
  (let [intcode (assoc (i/parse-intcode input) 0 2)
        in-chan (chan 20) out-chan (chan 20)
        state (i/start intcode in-chan out-chan)]
    (i/drain-until-closed (a/transduce (x/partition 3) process-output (init-game state) out-chan))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))