(ns day5
  (:use clojure.repl
        clojure.test))

(def input (slurp (clojure.java.io/resource "day5")))

(defn to-vector [input]
  (vec (map read-string (clojure.string/split input #","))))

(defn add-op
  [input addr1 addr2 addr3]
  )
