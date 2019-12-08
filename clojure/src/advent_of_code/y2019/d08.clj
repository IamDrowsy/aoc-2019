(ns advent-of-code.y2019.d08
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [clojure.string :as str]))

(def day 8)
(def width 25)
(def height 6)
(def pixel-per-layer (* width height))

(defn parse-layer [layer]
  (zipmap (for [x (range 25)
                y (range height)]
            [x y])
          (mapv #(parse-long (str %)) layer)))

; we build a vector of maps [x y] -> long
(defn parse-input [input]
  (mapv parse-layer (partition pixel-per-layer input)))

(defn solve-1 [input]
  (let [layers (parse-input input)
        layer-freqs (mapv #(frequencies (vals %)) layers)
        fewest-zero-layer-freqs (first (sort-by #(% 0) layer-freqs))]
    (* (fewest-zero-layer-freqs 1)
       (fewest-zero-layer-freqs 2))))

(defn p->str [p]
  (case p
    0 " "
    1 "X"
    "?"))

(defn layer-string [layer]
  (->>
    (for [x (range width)
          y (range height)]
      (layer [x y]))
    (partition width)
    (mapv #(str/join (map p->str %)))
    (str/join "\n")))

(defn merge-pixel [p1 p2]
  (if (= p1 2)
    p2
    p1))

(defn solve-2 [input]
  (let [layers (parse-input input)]
    (layer-string (apply (partial merge-with merge-pixel)
                         layers))))


(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))