(ns advent-of-code.util
  (:require [clj-http.client :as client]))

(defn parse-long [s]
  (Long/parseLong s))

(defn- get-input* [day]
  (let [session (slurp "resources/session")]
    (:body
      (client/get (format "https://adventofcode.com/2019/day/%s/input" day)
                  {:headers {"Cookie" (str "session=" session)}}))))

(def get-input (memoize get-input*))