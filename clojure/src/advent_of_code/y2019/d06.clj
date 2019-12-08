(ns advent-of-code.y2019.d06
  (:require [clojure.string :as str]
            [advent-of-code.util :refer [fixed-point parse-long get-input check]]
            [criterium.core :refer [quick-bench]]
            [neo4j-clj.core :as db]))

(def day 6)

(defn dependency-map [input]
  (into {} (map #(vec (reverse (str/split % #"\)"))) (str/split-lines input))))

(defn dep-map->nodes [dep-map]
  (let [all (into #{} (concat (vals dep-map) (keys dep-map)))]
    (str/join ",\n" (map #(str "(NODE" % "{name: '"% "'})") all))))

(defn dep-map->relations [dep-map]
  (str/join ",\n" (map (fn [[k v]]
                         (str "(NODE" k ")-[:ORBITS]->(NODE" v ")"))
                     dep-map)))

(defn input->query [input]
  (let [m (dependency-map input)]
    (str "CREATE " (dep-map->nodes m) ",\n" (dep-map->relations m))))

(defn fill-db [db input]
  (db/execute (db/get-session db)
              (input->query input)))

(defmacro with-filled-db [input run-fun]
  `(let [db# (db/create-in-memory-connection)
         _# (fill-db db# ~input)
         result# (~run-fun db#)]
     ((:destroy-fn db#))
     result#))

(db/defquery all-nodes "MATCH (x) return x")
(db/defquery relation-count "MATCH (x {name: $name}) -[*]->(y) return count(y)")

(defn solve-1 [input]
  (with-filled-db input
                  (fn [db]
                    (let [all (map :x
                                   (with-open [session (db/get-session db)]
                                     (all-nodes session)))
                          result
                          (with-open [session (db/get-session db)]
                            (reduce (fn [sum name]
                                      (+ sum (first (vals (first (relation-count session name))))))
                                    0
                                    all))]
                      result))))

(defn run-2 [db]
  (with-open [session (db/get-session db)]
    (-> (db/execute session
                    "MATCH (start {name:'SAN'}),(end {name:'YOU'}), p = shortestPath((start)-[*]-(end))
                     RETURN p")
        first
        :p
        (#(- (.size %) 2)))))

(defn solve-2 [input]
  (with-filled-db input
                  run-2))


(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))

