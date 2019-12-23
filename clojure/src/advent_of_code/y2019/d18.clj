(ns advent-of-code.y2019.d18
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [neo4j-clj.core :as db]
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]
    [com.rpl.specter :as s]
    [clojure.set :as set]
    [flatland.useful.seq :as us]))

(def day 18)

(defn parse-grid [input]
  (apply merge (map #(into {} %)
                    (map-indexed (fn [y line]
                                   (map-indexed (fn [x cell]
                                                  [[x y] cell])
                                                line))
                                 (str/split-lines input)))))

(defn val->label [val]
  (cond (= val \#) "BLOCKED"
        (= val \.) "WALKABLE"
        (#{\@ \1 \2 \3 \4} val) "WALKABLE:START"
        (= (str val) (str/upper-case val)) "WALKABLE:DOOR"
        :else "WALKABLE:KEY"))

(defn node-name [x y]
  (str "NODE_" x "_" y))

(defn connection [type [source-x source-y] [target-x target-y]]
  (str "(" (node-name source-x source-y) ")-[:" type "]->(" (node-name target-x target-y) ")"))

(defn neighbors [grid]
  (let [max-x (first (last (sort (keys grid))))
        max-y (second (last (sort-by second (keys grid))))]
    (str/join ",\n"
              (for [y (range 0 max-y)
                    x (range 0 max-x)]
                (str/join ",\n" (map #(connection "NEIGHBOR" [x y] %)
                                     [[(inc x) y] [x (inc y)]]))))))

(defn grid-nodes [grid]
  (str/join ",\n"
            (map
              (fn [[[x y] v]] (str "(" (node-name x y) ":" (val->label v) " {x: " x ", y: " y ", val: '" v "'})"))
              grid)))

(defn create-full-grid [grid]
  (str "CREATE " (grid-nodes grid) ",\n" (neighbors grid)))

(db/defquery delete-blocked-path
             "MATCH (:BLOCKED)-[r:NEIGHBOR]-() DELETE r")

(defn fill-db [db grid]
  (with-open [session (db/get-session db)]
    (db/execute session (create-full-grid grid))
    (delete-blocked-path session)))

(db/defquery  key-to-key-query
             "MATCH p=shortestPath((k {val: $k1})-[*]-(k2:KEY {val: $k2}))
              RETURN [length(p),k,k2, [n IN nodes(p) WHERE labels(n)=['WALKABLE','DOOR']]]")

(db/defquery all-keys-query
             "MATCH (k:KEY) RETURN k.val")

(defn key->key [db [k1 k2]]
  (with-open [session (db/get-session db)]
    (if-let [result (first (key-to-key-query session {:k1 k1 :k2 k2}))]
      (let [[length k1 k2 doors] (val (first result))]
        {:k1     (:val k1)
         :k2     (:val k2)
         :length length
         :doors  (map :val doors)}))))

(defn all-keys [db]
  (with-open [session (db/get-session db)]
    (map (keyword "k.val") (all-keys-query session))))

(defn all-key->key [db]
  (let [all (all-keys db)
        key-combos (combo/combinations (into [\@ \1 \2 \3 \4] all) 2)]
    (doall (remove nil? (map (partial key->key db) key-combos)))))

(defn all-locations [all-k->k]
  (->> (merge-with #(into %1 %2) (group-by :k1 all-k->k) (group-by :k2 all-k->k))
       (s/transform [s/MAP-VALS s/ALL :doors] set)
       (s/transform [s/MAP-VALS s/ALL :doors s/ALL] str/lower-case)
       (s/transform [s/ALL (s/collect-one s/FIRST) s/LAST s/ALL]
                    (fn [k m] (set/rename-keys m (if (= (:k1 m) k)
                                                   {:k1 :source
                                                    :k2 :target}
                                                   {:k2 :source
                                                    :k1 :target}))))))

(def start
  {:length 0
   :locations ["@"]
   :keys #{"@"}})

(defn not-collected [keys target-node]
  (not (keys (:target target-node))))

(defn needed-keys [keys target-node]
  (set/subset? (:doors target-node) keys))

(defn take-step [keys current-length locations {:keys [length target source]}]
  {:length (+ current-length length)
   :keys (conj keys target)
   :locations (s/setval [s/ALL (s/pred= source)] target locations)})

(defn possible-steps* [all {:keys [locations keys length]}]
  (mapv #(take-step keys length locations %)
       (s/select [(s/submap locations) s/MAP-VALS s/ALL
                  #(not-collected keys %)
                  #(needed-keys keys %)]
                 all)))

(defn possible-steps [all locations]
  (reduce into (pmap (partial possible-steps* all) locations)))

(defn remove-longer-routes [locations]
  (vals (us/groupings (juxt :keys :locations) #(if (< (:length %1) (:length %2))
                                                 %1 %2) {:length Long/MAX_VALUE} locations)))

(defn best-n-possible-steps [all locations]
  (println "Found keys:" (count (:keys (first locations))) "Current locations:" (count locations))
  (remove-longer-routes (possible-steps all locations)))

(defn solve-1 [input]
  (let [db (db/create-in-memory-connection)
        _ (fill-db db (parse-grid input))
        all (all-locations (all-key->key db))
        keys (count (all-keys db))
        f (partial best-n-possible-steps all)]
    ((:destroy-fn db))
    (:length (first (sort-by :length (first (drop keys (iterate f [start]))))))))

(def grid-update
  {[39 39] \1 [40 39] \# [41 39] \2
   [39 40] \# [40 40] \# [41 40] \#
   [39 41] \3 [40 41] \# [41 41] \4})

(def start-2
  {:locations ["1" "2" "3" "4"]
   :length 0
   :keys #{"1" "2" "3" "4"}})

(defn solve-2 [input]
  (let [db (db/create-in-memory-connection)
        _ (fill-db db (merge (parse-grid input) grid-update))
        all (all-locations (all-key->key db))
        keys (count (all-keys db))
        f (partial best-n-possible-steps all)]
    ((:destroy-fn db))
    (:length (first (sort-by :length (first (drop keys (iterate f [start-2]))))))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))