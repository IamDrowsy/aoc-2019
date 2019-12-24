(ns advent-of-code.y2019.d12
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [clojure.string :as str]
    [medley.core :refer [map-vals]]
    [advent-of-code.util :as u]
    [fastmath.core :as fm]
    [medley.core :as m]))

(def day 12)

(defn parse-moon [moon]
  (let [parsed (re-matches #"<x=([^,]+), y=([^,]+), z=([^,]+)>" moon)]
    {:x (parse-long (parsed 1)) :y (parse-long (parsed 2)) :z (parse-long (parsed 3))
     :vx 0 :vy 0 :vz 0}))

(defn parse-input [input]
  {:moons (mapv parse-moon (str/split-lines input))})

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

(defn detect-cycle [index universe ckw vkw]
  (if (and (not (ckw universe))
           (every? zero? (map vkw (:moons universe))))
    (assoc universe ckw (inc index))
    universe))

(defn detect-cycles [index universe]
  (reduce (fn [u [ckw vkw]]
            (detect-cycle index u ckw vkw))
          universe
          [[:cx :vx] [:cy :vy] [:cz :vz]]))

(defn step [index universe]
  (-> universe
      (update :moons step-moons)
      ((partial detect-cycles index))))

(defn total-energy [universe]
  (reduce (fn [^long sum {:keys [^long x ^long y ^long z ^long vx ^long vy ^long vz]}]
            (+ sum
               (* (+ (Math/abs x) (Math/abs y) (Math/abs z))
                  (+ (Math/abs vx) (Math/abs vy) (Math/abs vz)))))
          0
          universe))

(defn solve-1 [input]
  (total-energy (:moons (nth (u/iterate-indexed step (parse-input input)) 1000))))

(defn found-all-cycles? [{:keys [cx cy cz]}]
  (and cx cy cz))

(defn full-cycle [{:keys [cx cy cz]}]
  (reduce fm/lcm [cx cy cz]))

(defn solve-2 [input]
    (* 2 (full-cycle (m/find-first found-all-cycles? (u/iterate-indexed step (parse-input input))))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))