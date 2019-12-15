(ns advent-of-code.y2019.d12
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [clojure.string :as str]
    [clojure.core.async :as a :refer [thread >!! <!! chan]]
    [advent-of-code.y2019.intcode :as i]))

(def day 12)

(defn parse-moon [index moon]
  (let [parsed (re-matches #"<x=([^,]+), y=([^,]+), z=([^,]+)>" moon)]
    {:name index
     :x (parse-long (parsed 1)) :y (parse-long (parsed 2)) :z (parse-long (parsed 3))}))
(defn parse-input [input]
  (map-indexed parse-moon (str/split-lines input)))

(defn apply-gravity [my-x their-x my-vx]
  (cond (= my-x their-x) my-vx
        (< my-x their-x) (inc my-vx)
        (> my-x their-x) (dec my-vx)))

(defn run-moon [universe-chan mult {:keys [name x y z]}]
  (println "Starting moon" name)
  (let [in-chan (chan 20)]
    (a/tap mult in-chan)
    (thread
      (loop [x x y y z z vx 0 vy 0 vz 0]
        (let [command (<!! in-chan)]
          (case (:command command)
            :request-report (do
                              (>!! (:answer-chan command) {:command :full-report :name name :x x :y y :z z :vx vx :vy vy :vz vz})
                              (recur x y z vx vy vz))
            :move (recur (+ x vx) (+ y vy) (+ z vz) vx vy vz)
            :report-pull (do (>!! universe-chan {:command :apply-pull
                                                 :x       x :y y :z z})
                             (recur x y z vx vy vz))
            :apply-pull (do (>!! universe-chan {:command :applied-pull})
                            (recur x y z
                                   (apply-gravity x (:x command) vx)
                                   (apply-gravity y (:y command) vy)
                                   (apply-gravity z (:z command) vz)))
            (recur x y z vx vy vz)))))))

(defn create-universe [input]
  (let [moons (parse-input input)
        universe-input (chan 100)
        mult (a/mult universe-input)
        universe-output (chan 20)]
    (a/tap mult universe-output)
    (doseq [moon moons]
      (run-moon universe-input mult moon))
    {:moon-count (count moons)
     :universe-output universe-output
     :universe-input universe-input}))

(defn wait-pulls! [universe-output moon-count]
  (loop [missing (* moon-count moon-count)]
    (if (zero? missing)
      :done
      (do (<!! universe-output)
          (recur (dec missing))))))

(defn run-universe-step [{:keys [universe-input universe-output moon-count]}]
  (>!! universe-input {:command :report-pull})
  (wait-pulls! universe-output moon-count)
  (>!! universe-input {:command :move}))

(defn collect-full-report [universe-output moon-count]
  (loop [missing moon-count
         collected []]
    (if (zero? missing)
      collected
      (let [msg (<!! universe-output)]
        (if (= :full-report (:command msg))
          (recur (dec missing) (conj collected msg))
          (recur missing collected))))))

(defn full-report [{:keys [universe-input moon-count]}]
  (let [answer-chan (chan 20)]
    (>!! universe-input {:command     :request-report
                         :answer-chan answer-chan})
    (sort-by :name (collect-full-report answer-chan moon-count))))

(defn total-energy [full-report]
  (reduce (fn [sum {:keys [^long x ^long y ^long z ^long vx ^long vy ^long vz]}]
            (+ sum
               (* (+ (Math/abs x) (Math/abs y) (Math/abs z))
                  (+ (Math/abs vx) (Math/abs vy) (Math/abs vz)))))
          0
          full-report))

(defn solve-1 [input]
  (let [u (create-universe input)]
    (doseq [i (range 10)]
     #_(if (zero? (quot i 10)))
      #_(println (full-report u))
      (run-universe-step u))
    (total-energy (full-report u))))

(defn solve-2 [input]
  (first (<!! (i/start (i/parse-intcode input) [2]))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))