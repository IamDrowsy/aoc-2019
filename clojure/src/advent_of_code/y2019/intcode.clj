(ns advent-of-code.y2019.intcode
  (:require [advent-of-code.util :as u]
            [clojure.string :as str]
            [clojure.core.async :as a :refer [<! >! >!! <!! put! take! chan to-chan close! go go-loop thread]])
  (:import (clojure.lang IPersistentMap)))

(defprotocol IntcodeMemory
  (get-absolute [this idx])
  (set-absolute [this idx val])
  (get-pointer [this])
  (set-pointer [this newtval]))

(defprotocol IntcodeIO
  (read-input! [this])
  (write-output [this val])
  (set-input [this vals])
  (get-output [this]))

(defn init-intcode [input memory-string]
  {:io     {:input (to-chan input) :output (chan 20)}
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
  (get-absolute [this idx]
    #_(println "Get" idx "=" (get-in this [:memory idx]))
    (get-in this [:memory idx]))
  (set-absolute [this idx val]
    #_(println "Set" idx "to" val)
    (assoc-in this [:memory idx] val))
  (get-pointer [this]
    (get this :pointer))
  (set-pointer [this val]
    #_(println "Set Pointer to " val)
    (assoc this :pointer val))
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

(defn get-relative [intcode index]
  (get-absolute intcode (get-absolute intcode index)))

(defn set-relative [intcode index val]
  (set-absolute intcode (get-absolute intcode index) val))

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
      [\0 :get] #(get-relative % (+ (inc index) (get-pointer %)))
      [\0 :set] #(set-relative %1 (+ (inc index) (get-pointer %)) %2)
      [\1 :get] #(get-absolute % (+ (inc index) (get-pointer %)))
      [\1 :set] #(set-absolute %1 (+ (inc index) (get-pointer %)) %2))))

(defn opcode->fn* [opcode]
  (let [full-opcode (parse-opcode opcode)
        arg-fns (map-indexed #(partial (resolve-arg-mode full-opcode %1 %2)) (:args full-opcode))]
      (fn [intcode]
        #_(println "Opcode" opcode)
        (-> ((:fn full-opcode) intcode arg-fns)
            (inc-pointer (inc (count arg-fns)))))))

(def opcode->fn (memoize opcode->fn*))

(defn step [intcode]
  ((opcode->fn* (get-absolute intcode (get-pointer intcode))) intcode))

(defn run [intcode]
    (first (drop-while #(not= -1 (get-pointer %))
                       (iterate step intcode))))

(defn start
  ([intcode-memory input]
   (start intcode-memory input (chan 20)))
  ([intcode-memory input out-chan]
   (thread
     (loop [intcode {:io      {:input  (if (coll? input)
                                         (to-chan input)
                                         ;probably a chan
                                         input)
                               :output out-chan}
                     :memory  intcode-memory
                     :pointer 0}]
       (if (= -1 (get-pointer intcode))
         (get-output intcode)
         (recur (step intcode)))))))



