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

(defn range-of-points
  [point1 point2]
  (if (< point1 point2)
    (range point1 (inc point2))
    (range point2 (inc point1))))

(defn iter-move
  [pos move]
  (for [x (range-of-points (first pos) (first move))
        y (range-of-points (second pos) (second move))]
    [x y]))

(defn iter-next-step
  [pos next-step]
  (let [move (matrix-move pos next-step)]
    (iter-move pos move)))

(eval-step "R192")
(defn steps-into-coords
  [input]
  (reduce #(conj %1 (matrix-move (last %1) %2)) [[0 0]] input))
(def coordlist1 (steps-into-coords n1))
(def coordlist2 (steps-into-coords n2))
;; (reduce #(do (println %1 %2) (+ %1 (first (eval-step %2)))) 0 n1)

(defn compute-and-set
  [coordlist]
  (loop [coordlist coordlist
         coord-set #{}
         pos [0 0]]
    (if (empty? coordlist)
      coord-set
      (recur (rest coordlist)
             (into coord-set (iter-move pos (first coordlist)))
             (first coordlist)))))

(def coordset1 (compute-and-set coordlist1))
(def coordset2 (compute-and-set coordlist2))
(def section (clojure.set/intersection coordset1 coordset2))

(defn xy-sum
  [pos]
  (cond
    (neg? (first pos)) (recur [(* -1 (first pos)) (second pos)])
    (neg? (second pos)) (recur [(first pos) (* -1 (second pos))])
    :else (+ (first pos) (second pos))))

(sort (map xy-sum section))
