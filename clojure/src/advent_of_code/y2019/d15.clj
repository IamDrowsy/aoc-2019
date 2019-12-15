(ns advent-of-code.y2019.d15
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [clojure.string :as str]
    [clojure.core.async :as a :refer [thread >!! <!! chan]]
    [advent-of-code.y2019.intcode :as i]
    [medley.core :refer [remove-keys map-vals filter-vals find-first]]))

(def day 15)

(defn needed-points [known-points [cur-x cur-y]]
  (remove-keys known-points
               {[cur-x (inc cur-y)] 1 [cur-x (dec cur-y)] 2
                [(dec cur-x) cur-y] 3 [(inc cur-x) cur-y] 4}))

(defn ->output-at-pos [droid input]
  (let [in (chan 20)
        out (chan 20)
        _ (>!! in input)
        new-droid (i/resume @droid in out)
        output (<!! out)]
    [output droid]))

(defn spawn-droid [base-droid input]
  (let [in (chan 20)
        _ (>!! in input)]
    (i/resume @base-droid in)))

(defn spawn-droids* [known-points [pos droid]]
  (map-vals (partial spawn-droid droid) (needed-points known-points pos)))

(defn spawn-droids [known-points current-droids]
  (reduce (fn [m entry]
            (merge m (spawn-droids* known-points entry)))
          {} current-droids))

(defn droid-map->new-map [droid-map]
  (map-vals i/next-output! droid-map))

(defn stop-droid [droid-map pos]
  (if-let [droid (droid-map pos)]
    (i/stop-and-drain droid))
  (dissoc droid-map pos))

(defn update-droid-map [droid-map new-map]
  (let [pos-to-stop (keys (filter-vals zero? new-map))]
    (reduce stop-droid droid-map pos-to-stop)))

(defn map-region [{:keys [current-droids known-points steps]}]
  (let [droid-map (spawn-droids known-points current-droids)
        new-map (droid-map->new-map droid-map)]
    {:current-droids (update-droid-map droid-map new-map)
     :known-points (merge known-points new-map)
     :steps (inc steps)}))

(defn located-oxygen-system? [{:keys [known-points]}]
  (first (find-first (fn [[k v]] (= 2 v)) known-points)))

(defn init [input]
  {:steps 0
   :known-points {[0 0] 1}
   :current-droids {[0 0] (i/start (i/parse-intcode input))}})

(defn find-oxygen-system [input]
  (first (drop-while (complement located-oxygen-system?)
                     (iterate map-region (init input)))))

(defn solve-1 [input]
  (:steps (find-oxygen-system input)))

(defn not-fully-explored [{:keys [current-droids]}]
  (not (empty? current-droids)))

(defn init-2 [input]
  (let [state-at-oxygen (find-oxygen-system input)
        loc (located-oxygen-system? state-at-oxygen)]
    {:steps 0
     :known-points {loc 2}
     :current-droids {loc ((:current-droids state-at-oxygen) loc)}}))

(defn solve-2 [input]
  (dec (:steps (first (drop-while not-fully-explored (iterate map-region (init-2 input)))))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))