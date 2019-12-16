(ns day3
  (:use clojure.repl
        clojure.test))

(def input (slurp (clojure.java.io/resource "day3")))

(defn split-line [input] (clojure.string/split input #"\n"))
(defn to-vector [input] (clojure.string/split input #",|\r"))

(def splitted-input (split-line input))
(def n1 (to-vector (first splitted-input)))
(def n2 (to-vector (second splitted-input)))

(defn eval-step
  [move]
  (let [direction (first move)
        steps (read-string (reduce #(str %1 %2) "" (rest move)))]
    (condp = direction
      \U [0 steps]
      \R [steps 0]
      \D [0 (* -1 steps)]
      \L [(* -1 steps) 0])))

(defn matrix-move
  [pos next-step]
  (let [move (eval-step next-step)]
    (vec (map + pos move))))

(eval-step "R192")
;; (reduce #(do (println %1 %2) (+ %1 (first (eval-step %2)))) 0 n1)
