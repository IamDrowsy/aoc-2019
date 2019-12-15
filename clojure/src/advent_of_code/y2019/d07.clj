(ns advent-of-code.y2019.d07
  (:require [clojure.string :as str]
            [advent-of-code.util :refer [parse-long get-input check]]
            [advent-of-code.y2019.intcode :as i]
            [clojure.set :as set]
            [criterium.core :refer [quick-bench]]
            [clojure.math.combinatorics :as combo]
            [clojure.core.async :as a :refer [<!! chan >!! mult tap poll!]]))

(def day 7)

(defn run-phase [start-intcode phase-setting input]
  (first (i/drain (i/start start-intcode [phase-setting input]))))

(defn run-full-cycle [start-intcode phase-settings]
  (reduce #(run-phase start-intcode %2 %1)
          0
          phase-settings))

(defn run-phase-setting [start-incode [A B C D E]]
  (first (<!! (let [A->B (chan 20) B->C (chan 20) C->D (chan 20) D->E (chan 20)]
                (>!! A->B B) (>!! B->C C) (>!! C->D D) (>!! D->E E)
                (i/start start-incode [A 0] A->B)
                (i/start start-incode A->B B->C)
                (i/start start-incode B->C C->D)
                (i/start start-incode C->D D->E)
                (i/start start-incode D->E)))))

(defn solve-1 [input]
  (let [intcode (i/parse-intcode input)]
    (last (sort (pmap (partial run-full-cycle intcode)
                      (combo/permutations [0 1 2 3 4]))))))

(defn solve-1-2 [input]
  (let [intcode (i/parse-intcode input)]
    (last (sort (map (partial run-phase-setting intcode)
                     (combo/permutations [0 1 2 3 4]))))))

(defn run-phase-setting-2 [start-incode [A B C D E]]
  (i/drain-until-closed
    (let [A->B (chan 20) B->C (chan 20) C->D (chan 20) D->E (chan 20) X->A (chan 20) E->X (chan 20)
          X->OUT (chan 20)
          m (mult E->X)]
      (>!! X->A A) (>!! X->A 0) (>!! A->B B) (>!! B->C C) (>!! C->D D) (>!! D->E E)
      (tap m X->A) (tap m X->OUT)
      (i/start start-incode X->A A->B)
      (i/start start-incode A->B B->C)
      (i/start start-incode B->C C->D)
      (i/start start-incode C->D D->E)
      (i/start start-incode D->E E->X)
      X->OUT)))

(defn solve-2 [input]
  (let [intcode (i/parse-intcode input)]
    (last (sort (mapcat (partial run-phase-setting-2 intcode)
                        (combo/permutations [5 6 7 8 9]))))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))

