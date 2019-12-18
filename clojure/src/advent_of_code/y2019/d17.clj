(ns advent-of-code.y2019.d17
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [clojure.string :as str]
    [clojure.core.async :as a :refer [thread >!! <!! chan]]
    [advent-of-code.y2019.intcode :as i]
    [neo4j-clj.core :as db]))

(def day 17)

(defn node-name [x y]
  (str "NODE" x "x" y))

(defn nodes [field width height]
  (str/join ",\n" (for [y (range height)
                        x (range width)]
                    (let [obj (field (+ x (* y width)))]
                      (str/join ",\n"
                                (remove nil?
                                        [(str "(" (node-name x y) " {x:" x ", y: " y ", solid: " (= 35 obj) "})")
                                         (when (< 0 x) (str "(" (node-name x y) ")-[:NEIGHBOR]->(" (node-name (dec x) y) ")"))
                                         (when (< 0 y) (str "(" (node-name x y) ")-[:NEIGHBOR]->(" (node-name x (dec y)) ")"))]))))))

(defn input->query [input]
  (let [camera (i/drain (i/start (i/parse-intcode input) []))
        width (count (first (partition-by #(= 10 %) camera)))
        field (->> camera (remove #(= 10 %)) (into []))
        height (/ (count field) width)]
    (str "CREATE "(nodes field width height))))

(defn fill-db [db input]
  (db/execute (db/get-session db)
              (input->query input))
  db)

(defmacro with-filled-db [input run-fun]
  `(let [db# (db/create-in-memory-connection)
         _# (fill-db db# ~input)
         result# (~run-fun db#)]
     ((:destroy-fn db#))
     result#))

(db/defquery cross-section
             "MATCH (cross {solid:true })--(y {solid: true})
              WITH cross, count(y) as c, cross.x * cross.y as align
              WHERE c = 4
              RETURN sum(align)")

(defn solve-1 [input]
  (with-filled-db input
                  (fn [db]
                    (with-open [session (db/get-session db)]
                      (val (ffirst (cross-section session)))))))


(defn run []
  (check day 1 (solve-1 (get-input day))))