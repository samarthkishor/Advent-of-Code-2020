(ns day2.core
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn- count-value
  [seq value]
  (reduce
   (fn [count e]
     (if (= (str e) (str value)) ;; HACK because char is not equal to string
       (inc count)
       count))
   0
   seq))

(defn- parse-line
  "Parses a line into a list (strings become symbols)"
  [line]
  (->> line
       (re-matches
        #"(\d+)-(\d+)\s+([a-z]):\s+([a-z]+)")
       rest
       (map edn/read-string)))

;; for part 1
(defn- valid-line?
  [line]
  (let [[least most letter passwd] (parse-line line)
        letter-count (count-value (str passwd) (str letter))]
    (<= least letter-count most)))

;; for part 2
(defn- new-valid-line?
  [line]
  (let [[first-position second-position letter passwd] (parse-line line)]
    (= 1
       (count-value
        [(= (str (nth (str passwd) (dec first-position))) (str letter))
         (= (str (nth (str passwd) (dec second-position))) (str letter))]
        true))))

(defn solve
  [f]
  (with-open [rdr (io/reader "input.txt")]
    (count-value
     (map f (line-seq rdr))
     true)))

(def part-1 (solve valid-line?))
(def part-2 (solve new-valid-line?))

(println "Part 1:" part-1)
(println "Part 2:" part-2)
