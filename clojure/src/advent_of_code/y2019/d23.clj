(ns advent-of-code.y2019.d23
  (:require
    [advent-of-code.util :refer [parse-long get-input check]]
    [criterium.core :refer [quick-bench]]
    [clojure.core.async :as a :refer [thread >!! <!! chan]]
    [advent-of-code.y2019.intcode :as i]))

(def day 23)

(def cpu-count 50)

(defn start-nic [intcode address]
  (let [in-chan (chan 20)
        _ (>!! in-chan address)]
    (i/start intcode in-chan)))

(defn start-nics [input]
  (let [intcode (i/parse-intcode input)]
    (mapv (partial start-nic intcode) (range cpu-count))))

(defn vconj
  [coll x y]
  (if (nil? coll)
    [x y]
    (conj coll x y)))

(defn indexed-alts!! [channels]
  (let [cl (zipmap channels (range (count channels)))
        [msg chan] (a/alts!! channels)]
    [msg chan (cl chan)]))

(defn read-next-package! [{:keys [ledger nics idle-nics]}]
  (let [out-chans (mapv i/get-outchan nics)
        [target outchan index] (indexed-alts!! out-chans)
        x (<!! outchan) y (<!! outchan)]
    (if (nil? target)
      nil
      (if (= target 255)
        (swap! ledger assoc target [x y])
        (do
          (swap! idle-nics disj index)
          (swap! ledger
                 #(update % target vconj x y)))))))

(defn init [input]
  {:ledger (atom {})
   :nics (start-nics input)
   :idle-nics (atom #{})})

(defn start-reader [state]
  (thread
    (println "Starting reader")
    (loop [result (read-next-package! state)]
      (if result
        (recur (read-next-package! state))
        (println "Stopped reader")))))

(defn send-input! [{:keys [ledger nics idle-nics]} address]
  (let [waiting-inputs (@ledger address)
        inchan (i/get-inchan (nics address))]
    (if (nil? waiting-inputs)
      (do (a/offer! inchan -1)
          (swap! idle-nics conj address))
      (do (a/onto-chan (i/get-inchan (nics address)) waiting-inputs false)
          (swap! ledger dissoc address)
          (swap! idle-nics disj address)))))

(defn next-requesting-channel [state]
  (let [channels (map i/get-requestchan (:nics state))
        [msg _ index] (indexed-alts!! channels)]
    (if (nil? msg)
      nil
      index)))

(defn start-sender [state]
  (thread
    (println "Starting sender")
    (loop [requesting-channel (next-requesting-channel state)]
      (if requesting-channel
        (do (send-input! state requesting-channel)
            (recur (next-requesting-channel state)))
        (println "Stopping sender ")))))

(defn tear-down [state]
  (doseq [nic (:nics state)]
    (a/close! (i/get-inchan nic))))

(defn start-system [input]
  (let [state (init input)]
    (start-reader state)
    (start-sender state)
    state))

(defn watch-for-255 [state]
  (thread (while (nil? (@(:ledger state) 255))
            (Thread/sleep 200))
          (tear-down state)
          (second (@(:ledger state) 255))))

(defn solve-1 [input]
  (let [system (start-system input)]
    (i/drain-until-closed (watch-for-255 system))
    (second (@(:ledger system) 255))))

(defn idle? [system]
  (and (= (count @(:idle-nics system)) cpu-count)
       (= (keys @(:ledger system)) [255])))

(defn run-nat [system]
  (thread
    (loop [last-send [nil nil]]
      (if (idle? system)
        (let [to-send (@(:ledger system) 255)]
          (println "detected idle, send" to-send)
          (swap! (:ledger system) assoc 0 to-send)
          (reset! (:idle-nics system) #{})
          (Thread/sleep 100)
          (if (= (second to-send) (second last-send))
            (do (tear-down system)
                (second last-send))
            (recur to-send)))
        (recur last-send)))))

(defn solve-2 [input]
  (let [system (start-system input)]
    (first (i/drain-until-closed (run-nat system)))))

(defn run []
  (check day 1 (solve-1 (get-input day)))
  (check day 2 (solve-2 (get-input day))))