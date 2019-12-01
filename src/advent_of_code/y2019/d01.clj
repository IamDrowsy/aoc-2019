(ns advent-of-code.y2019.d01
  (:require [clojure.string :as str]
            [advent-of-code.util :refer [parse-long]]
            [criterium.core :refer [quick-bench]]))

(def input "87201\n146745\n99430\n67664\n67482\n72598\n114480\n111866\n146672\n51465\n59100\n87507\n106993\n61427\n97982\n133329\n133510\n117352\n58800\n129228\n102366\n77934\n149630\n71567\n139965\n130271\n53259\n134032\n54158\n74679\n148463\n101585\n51744\n112537\n59140\n92980\n83174\n67797\n58890\n55849\n50424\n112780\n104595\n114465\n90528\n86503\n51661\n124689\n101081\n81478\n124821\n83420\n108738\n76506\n111217\n104897\n133747\n128808\n81563\n106688\n67129\n130968\n75576\n101197\n129318\n135015\n149839\n110182\n104687\n147803\n140555\n70447\n63224\n85143\n146115\n77789\n64003\n115257\n61397\n86873\n143481\n129785\n68764\n99388\n91050\n109136\n101777\n98104\n103643\n131374\n83808\n125949\n147277\n144448\n112673\n136408\n75776\n141630\n116821\n113349")

(defn fuel-requirement [weight]
  (- (quot weight 3) 2))

(defn solve-1 []
  (transduce (comp (map parse-long)
                   (map fuel-requirement))
             +
             (str/split-lines input)))

(defn total-fuel-requirement [weight]
  (transduce (comp (drop 1)
                   (take-while pos-int?))
             +
             (iterate fuel-requirement weight)))

(defn solve-2 []
  (transduce (comp (map parse-long)
                   (map total-fuel-requirement))
             +
             (str/split-lines input)))

(defn run []
  (assert (= 3361976 (solve-1)))
  (assert (= 5040085 (solve-2))))
