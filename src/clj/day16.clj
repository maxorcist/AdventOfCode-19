(ns day16
  (:use clojure.repl
        clojure.test))

(def input (slurp (clojure.java.io/resource "day16")))
(defn to-vector [input] (reduce #(conj %1 (- (int %2) 48)) [] input))

(def testinput (to-vector "80871224585914546619083218645595"))

(defn make-single-digit
  [n]
  (if (pos? n)
    (mod n 10)
    (mod (* -1 n) 10)))

(defn multiplier
  [digit times]
  (* digit times))
