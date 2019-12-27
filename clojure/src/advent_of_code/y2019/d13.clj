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

(def count-blocks
  (comp (x/partition 3)
        (filter #(= 2 (last %)))
        x/count))

(defn solve-1 [input]
  (let [out-chan (chan 20)]
    (i/start (i/parse-intcode input) [] out-chan)
    (<!! (a/transduce count-blocks + 0 out-chan))))

(defn init-game []
  (atom {:px 0 :py 0
         :bx 0 :by 0
         :score nil
         :blocks #{}}))

(defn needed-input [{:keys [bx px]}]
  (cond (< px bx) 1
        (< bx px) -1
        :else 0))

(defn process-output
  ([game]
   (:score @game))
  ([game [x y i]]
   (cond
     (and (= -1 x) (= 0 y)) (swap! game assoc :score i)
     (= i 3) (swap! game assoc :px x :py y)
     (= i 4) (swap! game assoc :bx x :by y)
     (= i 2) (swap! game update :blocks conj [x y])
     (= i 0) (if (contains? (:blocks @game) [x y])
               (swap! game update :blocks disj [x y])
               game)
     :else game)
   game))

(defn provide-input [intcode game]
  (thread
    (loop []
      (when (not (nil? (<!! (i/get-requestchan intcode))))
        (i/input! intcode (needed-input @game))
        (recur)))))

(defn solve-2 [input]
  (let [intcode (assoc (i/parse-intcode input) 0 2)
        in-chan (chan 20) out-chan (chan 20)
        intcode (i/start intcode in-chan out-chan)
        game (init-game)]
    (provide-input intcode game)
    (first (i/drain-until-closed (a/transduce (x/partition 3) process-output game out-chan)))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))