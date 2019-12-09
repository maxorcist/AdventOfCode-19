(ns day1
  (:require [clojure.java.io :as io])
  (:use [clojure.repl]))

(defn get-input [input] (slurp (io/resource input)))
(defn split-by-newline [input] (clojure.string/split input #"\n"))

(def data (split-by-newline (get-input "day1")))

(def testdata ["12" "14" "1969" "100756"])

(defn parse [x](Double/parseDouble x))
(defn remove-decimal [x] (int x))
(defn fuel-required [mass] (+ -2 (remove-decimal (/ mass 3))))
(defn string-coll-parser [stringcoll] (map parse stringcoll))

(def parsed-coll (map parse testdata))
(def mapped-fuel-coll (map fuel-required parsed-coll))
(defn mass-coll-mapper [masses] (map fuel-required (string-coll-parser masses)))
(defn mass-coll-reducer [masses] (reduce #(+ %1 (fuel-required (parse %2))) 0 masses))

(def result (mass-coll-reducer data))
(def testresult (mass-coll-reducer testdata))
;; Part two

(defn fuel-to-fuel-iterator [mass] (iterate fuel-required mass))
(defn iterated-fuel-coll [fuelmass](take-while pos? (fuel-to-fuel-iterator fuelmass)))
(defn sum-of-iterated-fuelmass [fuelmass] (reduce + (iterated-fuel-coll fuelmass)))

(defn parse-and-calc-input-to-fuel-of-fuel [data] (reduce #(+ %1 (sum-of-iterated-fuelmass %2)) 0 (mass-coll-mapper data)))
