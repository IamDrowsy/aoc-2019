(ns advent-of-code.y2019.d12
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [clojure.string :as str]
    [medley.core :refer [map-vals]]))

(def day 12)

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(defn parse-moon [moon]
  (let [parsed (re-matches #"<x=([^,]+), y=([^,]+), z=([^,]+)>" moon)]
    {:x (parse-long (parsed 1)) :y (parse-long (parsed 2)) :z (parse-long (parsed 3))
     :vx 0 :vy 0 :vz 0}))

(defn parse-input [input]
  {:moons (mapv parse-moon (str/split-lines input))
   :step 0})

(defn apply-gravity-for-dimension [me they dim vdim]
  (let [^long my-x (dim me) ^long their-x (dim they)]
    (cond (= my-x their-x) me
          (< my-x their-x) (update me vdim inc)
          (> my-x their-x) (update me vdim dec))))

(defn apply-gravity* [moon other-moon]
  (reduce (fn [m [dim vdim]] (apply-gravity-for-dimension m other-moon dim vdim))
          moon
          [[:x :vx] [:y :vy] [:z :vz]]))

(defn apply-gravity [moon other-moons]
  (reduce apply-gravity* moon other-moons))

(defn move [moon]
  (merge-with + moon {:x (:vx moon) :y (:vy moon) :z (:vz moon)}))

(defn step-moons [moons]
  (->> moons
       (mapv #(apply-gravity % moons))
       (mapv move)))

(defn step [universe]
  (-> universe
      (update :moons step-moons)
      (update :step inc)))

(defn total-energy [universe]
  (reduce (fn [^long sum {:keys [^long x ^long y ^long z ^long vx ^long vy ^long vz]}]
            (+ sum
               (* (+ (Math/abs x) (Math/abs y) (Math/abs z))
                  (+ (Math/abs vx) (Math/abs vy) (Math/abs vz)))))
          0
          universe))

(defn solve-1 [input]
  (total-energy (:moons (nth (iterate step (parse-input input)) 1000))))

(defn solve-2 [input]
  (let [starting (parse-input input)]
    (:step (first (drop-while #(not= (:moons %) (:moons starting)) (drop 1 (iterate step starting)))))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))