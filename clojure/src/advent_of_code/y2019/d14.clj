(ns advent-of-code.y2019.d14
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [instaparse.core :as insta]
    [instaparse.transform :as instat]
    [com.rpl.specter :as s]
    [advent-of-code.util :as u]))

(def day 14)

(def reaction-grammar
  "<S> = REACTION+
  REACTION = INPUTS <'=>'> OUTPUT
  INPUTS = INGREDIENT+
  OUTPUT = INGREDIENT
  INGREDIENT = AMOUNT CHEM
  AMOUNT = #'\\d+'
  CHEM = #'\\w+'")

(defn parse-reaction [reaction-string]
  (-> (insta/parser reaction-grammar :auto-whitespace :comma)
      (insta/parse reaction-string)
      (as-> x (instat/transform {:INGREDIENT (fn [[_ amount] [_ chem]] [chem (parse-long amount)])
                                 :REACTION   (fn [[_ & inputs] [_ & output]]
                                               [(first (first output))
                                                {:amount (second (first output))
                                                 :inputs (into {} inputs)}])}
                                x))
      (as-> x (into {} x))))

(defn production [reactions fuel]
  {:reactions reactions
   :needed {"FUEL" fuel}
   :stored {}})

(defn produce-single [{:keys [reactions needed stored]} chem]
  (let [stored-amount (stored chem 0)
        needed-amount (needed chem)
        to-produce-amount (max (- needed-amount stored-amount))
        {:keys [amount inputs]} (reactions chem)
        needed-reactions (long (Math/ceil (/ to-produce-amount amount)))
        needed-ingredients (if (zero? to-produce-amount)
                             {}
                             (s/transform s/MAP-VALS #(* needed-reactions %) inputs))
        produced-amount (* needed-reactions amount)
        leftover (- (+ stored-amount produced-amount) needed-amount)]
    {:reactions reactions
     :needed (dissoc (merge-with + needed needed-ingredients) chem)
     :stored (s/setval [(s/keypath chem)] (if (zero? leftover) s/NONE leftover) stored)}))

(defn produce-step [production]
  (produce-single production (first (keys (dissoc (:needed production) "ORE")))))

(defn not-only-ore? [production]
  (or (< 1 (count (:needed production)))
      (not ((:needed production) "ORE"))))

(defn needed-ore [reactions fuel]
  ((:needed (first (drop-while not-only-ore? (iterate produce-step (production reactions fuel)))))
   "ORE"))

(defn solve-1 [input]
  (needed-ore (parse-reaction input) 1))

(defn solve-2 [input]
  (let [leftover-ore 1000000000000
        reactions (parse-reaction input)
        ore-per-fuel (needed-ore reactions 1)
        estimate (quot leftover-ore ore-per-fuel)]
    (dec (u/binary-search (partial needed-ore reactions) #(> % leftover-ore) estimate))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))