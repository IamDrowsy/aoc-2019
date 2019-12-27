(ns advent-of-code.y2019.d20
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [clojure.string :as str]
    [neo4j-clj.core :as db]
    [flatland.useful.seq :refer [groupings]]
    [com.rpl.specter :as s]
    [medley.core :refer [filter-vals find-first]]
    [meander.epsilon :as m]))

(def day 20)

(defn parse-grid [input]
  (filter-vals (fn [v] (re-matches #"\w|\." (str v)))
               (apply merge (map #(into {} %)
                                 (map-indexed (fn [y line]
                                                (map-indexed (fn [x cell]
                                                               [[x y] cell])
                                                             line))
                                              (str/split-lines input))))))

(defn val->label [val]
  (cond (= val \#) "BLOCKED"
        (= val \.) "WALKABLE"
        (= val \space ) "BLOCKED"
        :else "PORTAL"))

(defn node-name [x y]
  (str "NODE_" x "_" y))

(defn connection [type [source-x source-y] [target-x target-y]]
  (str "(" (node-name source-x source-y) ")-[:" type "]->(" (node-name target-x target-y) ")"))

(defn neighbors [grid]
  (let [max-x (inc (first (last (sort (keys grid)))))
        max-y (inc (second (last (sort-by second (keys grid)))))]
    (str/join ",\n"
              (remove empty?
                      (for [y (range 0 max-y)
                            x (range 0 max-x)
                            :when (grid [x y])]
                        (str/join ",\n" (map #(connection "NEIGHBOR" [x y] %)
                                             (filter grid [[(inc x) y] [x (inc y)]]))))))))

(defn grid-nodes [grid]
  (str/join ",\n"
            (map
              (fn [[[x y] v]] (str "(" (node-name x y) ":" (val->label v) " {x: " x ", y: " y ", val: '" v "'})"))
              grid)))

(defn create-full-grid [grid]
  (str "CREATE " (grid-nodes grid) ",\n" (neighbors grid)))

(db/defquery join-portal-names
             "MATCH (p1:PORTAL)-->(p2:PORTAL) WITH  p1,p2,p1.val + p2.val as pname
              SET p2.val=pname, p1.val=pname")

(db/defquery remove-double-portals
             "MATCH (p:PORTAL)
             WHERE NOT (p)--(:WALKABLE)
             DETACH DELETE p")

(db/defquery create-portals
             "MATCH (w:WALKABLE)--(p:PORTAL),(p2:PORTAL {val: p.val})--(w2:WALKABLE) CREATE (w)-[:PORTAL]->(w2)")

(db/defquery mark-start-and-end
             "MATCH (start:PORTAL {val: 'AA'}), (end:PORTAL {val: 'ZZ'}) REMOVE start:PORTAL, end:PORTAL SET start:START, end:END")

(db/defquery mark-outer-portals
             "MATCH (p:PORTAL)
              WHERE p.x = 1 OR p.x = $MAX_X OR p.y = 1 OR p.y = $MAX_Y
              WITH p.val + '_O' AS pname, p AS p
              SET p.val = pname")

(db/defquery mark-inner-portals
             "MATCH (p:PORTAL)
              WHERE p.x > 1 AND p.x < $MAX_X AND p.y > 1 AND p.y < $MAX_Y
              WITH p.val + '_I' AS pname, p AS p
              SET p.val = pname")


(defn fill-db [db grid]
  (with-open [session (db/get-session db)]
    (db/execute session (create-full-grid grid))
    (join-portal-names session)
    (remove-double-portals session)))


(db/defquery shortest-path
             "MATCH (:START)--(s:WALKABLE), (z:WALKABLE)--(:END), p=shortestPath((s)-[*]-(z)) RETURN length(p)")

(defn solve-1 [input]
  (let [grid (parse-grid input)
        db (db/create-in-memory-connection)
        _ (fill-db db grid)
        result (with-open [session (db/get-session db)]
                 (mark-start-and-end session)
                 (create-portals session)
                 (shortest-path session))]
    ((:destroy-fn db))
    (val (first (first result)))))

(db/defquery add-flipped-neighbors
             "MATCH (x)-[:NEIGHBOR]->(y)
              CREATE (y)-[:NEIGHBOR]->(x)")

(db/defquery shortest-recursiv-path
             "MATCH (:START)-->(s:WALKABLE), (z:WALKABLE)-->(:END), p=shortestPath((s)-[*]->(z))
              WHERE length([r IN relationships(p) WHERE type(r) = 'DOWN']) = length([r IN relationships(p) WHERE type(r) = 'UP'])
                    AND length([r IN relationships(p) WHERE type(r) = 'DOWN']) < 50
                    AND length([r IN relationships(p) WHERE type(r) = 'UP']) < 50
              RETURN length(p), p")

(db/defquery all-path-query
             "MATCH (p1:PORTAL),(p2:PORTAL), p=shortestPath((p1)-[*]-(p2))
              WHERE p1.val <> p2.val
              WITH p1 AS p1, p2 AS p2, length(p) AS length
              RETURN p1,p2,length")

(defn portal-dir? [max-x max-y x y]
  (if (and (< 1 x max-x) (< 1 y max-y))
    1 -1))

(defn all-paths [db max-x max-y]
  (groupings :source
             #(dissoc % :source)
    (m/search (with-open [session (db/get-session db)]
                (all-path-query session))
              (m/scan {:p1 {:val ?p1 :x ?x1 :y ?y1} :p2 {:val ?p2 :x ?x2 :y ?y2} :length ?length})
              {:source ?p1 :target ?p2 :length ?length :dir (portal-dir? max-x max-y ?x2 ?y2)})))


(defn start [all-paths]
  {:locations [{:location "AA_O" :layer 0 :length 0}]
   :known-locations {0 {"AA_O" 0}}
   :paths all-paths})


(defn possible-steps [paths location layer]
  (s/select [(s/keypath location) s/ALL #(if (zero? layer)
                                           (or (= (:dir %) 1) (= (:target %) "ZZ_O"))
                                           true)]
            paths))

(defn i<->o [location]
  (if (str/ends-with? location "I")
    (str (subs location 0 2) "_O")
    (str (subs location 0 2) "_I")))

(defn next-locations* [paths {:keys [location layer length]}]
  (map (fn [x]
         (if (and (= (:target x) "ZZ_O") (zero? layer))
           {:location "ZZ_O" :layer 0 :length (- (+ length (:length x)) 2)}
           {:location (i<->o (:target x)) :layer (+ layer (:dir x)) :length (dec (+ length (:length x)))}))
       (possible-steps paths location layer)))

(defn next-locations [{:keys [paths locations]}]
  (mapcat (partial next-locations* paths) locations))

(defn better-alternativ? [{:keys [known-locations]} {:keys [location layer length]}]
  (<= (get-in known-locations [layer location] Long/MAX_VALUE) length))

(defn update-known-locations [known-locations new-locations]
  (reduce (fn [known new]
            (assoc-in known [(:layer new) (:location new)] (:length new)))
          known-locations
          (reverse(sort-by :length new-locations))))

(defn step [state]
  (let [next (next-locations state)
        new-next (remove (partial better-alternativ? state) next)]
    (-> state
        (assoc :locations new-next)
        (update :known-locations update-known-locations new-next))))

(defn found-way? [state]
  (get-in state [:known-locations 0 "ZZ_O"]))

(defn solve-2 [input]
  (let [grid (parse-grid input)
        max-x (dec (last (sort (map first (keys grid)))))
        max-y (dec (last (sort (map second (keys grid)))))
        db (db/create-in-memory-connection)
        _ (fill-db db grid)
        _ (with-open [session (db/get-session db)]
            (mark-outer-portals session {:MAX_X max-x :MAX_Y max-y})
            (mark-inner-portals session {:MAX_X max-x :MAX_Y max-y}))
        all-paths (all-paths db max-x max-y)]
    ((:destroy-fn db))
    (found-way? (find-first found-way? (iterate step (start all-paths))))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))