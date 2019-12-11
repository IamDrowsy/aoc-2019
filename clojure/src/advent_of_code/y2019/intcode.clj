(ns advent-of-code.y2019.intcode
  (:require [advent-of-code.util :as u]
            [clojure.string :as str]
            [clojure.core.async :as a :refer [<! >! >!! <!! put! take! chan to-chan close! go go-loop thread]])
  (:import (clojure.lang IPersistentMap)))

(defprotocol IntcodeMemory
  (get-immediate [this idx])
  (set-immediate [this idx val])
  (get-pointer [this])
  (set-pointer [this newtval])
  (get-relative-base [this])
  (set-relative-base [this new-val]))

(defprotocol IntcodeIO
  (read-input! [this])
  (write-output [this val])
  (set-input [this vals])
  (get-output [this]))

(defn init-intcode [input memory-string]
  {:io     {:input (to-chan input) :output (chan 20)}
   :relative-base 0
   :memory (zipmap (range) (map u/parse-long (str/split memory-string #",")))
   :pointer 0})

(defn parse-intcode [memory-string]
  (zipmap (range) (map u/parse-long (str/split memory-string #","))))

(defn close-and-drain [chan]
  (close! chan)
  (into [] (take-while identity (repeatedly #(a/poll! chan)))))

(defn drain-until-closed [chan]
  (into [] (take-while identity (repeatedly #(<!! chan)))))


(extend-type IPersistentMap
  IntcodeMemory
  (get-immediate [this idx]
    #_(println "Get" idx "=" (get-in this [:memory idx]))
    (get-in this [:memory idx] 0))
  (set-immediate [this idx val]
    #_(println "Set" idx "to" val)
    (assoc-in this [:memory idx] val))
  (get-pointer [this]
    (get this :pointer))
  (set-pointer [this val]
    #_(println "Set Pointer to " val)
    (assoc this :pointer val))
  (get-relative-base [this]
    (get this :relative-base))
  (set-relative-base [this val]
    (assoc this :relative-base val))
  IntcodeIO
  (read-input! [this]
    #_(println "Read Input")
    (<!! (get-in this [:io :input])))
  (write-output [this val]
    #_(println "Write" val "to output")
    (>!! (get-in this [:io :output]) val)
    this)
  (set-input [this vals]
    (assoc-in this [:io :input]
              (if (coll? vals) (to-chan vals) vals)))
  (get-output [this]
    (close-and-drain (get-in this [:io :output]))))

(defn inc-pointer [intcode inc-count]
  (set-pointer intcode (+ (get-pointer intcode) inc-count)))

(defn adjust-relative-base [intcode val]
  (set-relative-base intcode (+ (get-relative-base intcode)
                                val)))

(defn get-position [intcode index]
  (get-immediate intcode (get-immediate intcode index)))

(defn set-position [intcode index val]
  (set-immediate intcode (get-immediate intcode index) val))

(defn get-relative [intcode index]
  (get-immediate intcode (+ (get-immediate intcode index)
                            (get-relative-base intcode))))

(defn set-relative [intcode index val]
  (set-immediate intcode (+ (get-immediate intcode index)
                            (get-relative-base intcode)) val))

;fn should be incode, getter/setter fn, which should be fns of intcode itself
(def commands
  {1  {:args [:get :get :set]
       :fn   (fn [i [g1 g2 s1]] (s1 i (+ (g1 i) (g2 i))))}
   2  {:args [:get :get :set]
       :fn   (fn [i [g1 g2 s1]] (s1 i (* (g1 i) (g2 i))))}
   3  {:args [:set]
       :fn   (fn [i [s]] (s i (read-input! i)))}
   4  {:args [:get]
       :fn   (fn [i [g]] (write-output i (g i)))}
   5  {:args [:get :get]
       :fn   (fn [i [g1 g2]] (if (zero? (g1 i))
                               i
                               (set-pointer i (- (g2 i) 3))))}
   6  {:args [:get :get]
       :fn   (fn [i [g1 g2]] (if (zero? (g1 i))
                               (set-pointer i (- (g2 i) 3))
                               i))}
   7  {:args [:get :get :set]
       :fn   (fn [i [g1 g2 s1]]
               (s1 i (if (< (g1 i) (g2 i)) 1 0)))}
   8  {:args [:get :get :set]
       :fn   (fn [i [g1 g2 s1]]
               (s1 i (if (= (g1 i) (g2 i)) 1 0)))}
   9  {:args [:get]
       :fn (fn [i [g1]]
             (adjust-relative-base i (g1 i)))}
   99 {:args [] :fn (fn [i _] (set-pointer i -2))}})

(defn parse-opcode [opcode]
  (let [op-str (str "0000000" opcode)
        command (u/parse-long (subs op-str (- (count op-str) 2)))
        args (count (:args (commands command)))]
    (merge (commands command)
      {:argmodes (vec (take args (drop 2 (reverse op-str))))
       :command command})))

(defn resolve-arg-mode [full-command index get-or-set]
  (let [mode ((:argmodes full-command) index)]
    (case [mode get-or-set]
      [\0 :get] #(get-position % (+ (inc index) (get-pointer %)))
      [\0 :set] #(set-position %1 (+ (inc index) (get-pointer %1)) %2)
      [\1 :get] #(get-immediate % (+ (inc index) (get-pointer %)))
      [\1 :set] #(set-immediate %1 (+ (inc index) (get-pointer %1)) %2)
      [\2 :get] #(get-relative % (+ (inc index) (get-pointer %)))
      [\2 :set] #(set-relative %1 (+ (inc index) (get-pointer %1)) %2))))

(defn opcode->fn* [opcode]
  (let [full-opcode (parse-opcode opcode)
        arg-fns (map-indexed #(partial (resolve-arg-mode full-opcode %1 %2)) (:args full-opcode))]
      (fn [intcode]
        #_(println opcode)
        (-> ((:fn full-opcode) intcode arg-fns)
            (inc-pointer (inc (count arg-fns)))))))

(def opcode->fn (memoize opcode->fn*))

(defn step [intcode]
  ((opcode->fn* (get-immediate intcode (get-pointer intcode))) intcode))

(defn run [intcode]
    (first (drop-while #(not= -1 (get-pointer %))
                       (iterate step intcode))))

(defn start
  ([intcode-memory input]
   (start intcode-memory input (chan 20)))
  ([intcode-memory input out-chan]
   (let [in-chan (if (coll? input) (to-chan input) input)]
     (thread
       (try
         (loop [intcode {:io      {:input  in-chan
                                   :output out-chan}
                         :memory  intcode-memory
                         :relative-base 0
                         :pointer 0}]
           (if (= -1 (get-pointer intcode))
             (get-output intcode)
             (recur (step intcode))))
         (catch Exception e
           (close! in-chan) (close! out-chan)
           (throw e)))))))



