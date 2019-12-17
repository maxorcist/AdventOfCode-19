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
    (reverse (range point2 (inc point1)))))

(defn iter-move
  [pos move]
  (for [x (range-of-points (first pos) (first move))
        y (range-of-points (second pos) (second move))]
    [x y]))

(defn iter-next-step
  [pos next-step]
  (let [move (matrix-move pos next-step)]
    (iter-move pos move)))

(defn steps-into-coords
  [input]
  (reduce #(conj %1 (matrix-move (last %1) %2)) [[0 0]] input))
(def coordlist1 (steps-into-coords n1))
(def coordlist2 (steps-into-coords n2))

(defn compute-and-set
  {:test (fn [] (is (= #{[0 0] [1 0] [2 0]} (compute-and-set [[0 0] [2 0] [0 0]]))))}
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

(def part-one-result (second (sort (map xy-sum section))))

(defn compute-til-stop
  {:test (fn [] (is (= 5 (compute-til-stop [[0 0] [10 0]] [5 0])))
           (is (= 7 (compute-til-stop [[0 0] [-10 0]] [-7 0])))
           (is (= 120 (compute-til-stop [[0 0] [100 0] [100 -50]] [100 -20])))
           (is (= 12 (compute-til-stop [[10 10] [10 20] [20 20]] [12 20]))))}
  [coordlist stop]
  (loop [coordlist coordlist
         counter 0]
    (let [iter-moves (iter-move (first coordlist) (second coordlist))]
      ;; (println (first coordlist) counter iter-moves)
      (if (or (empty? coordlist)
              (.contains iter-moves stop))
                  (+ counter (.indexOf iter-moves stop))
                  (recur (next coordlist)
                         (+ counter (dec (count iter-moves))))))))

(def testdata1 "R75,D30,R83,U83,L12,D49,R71,U7,L72")
(def testdata2  "U62,R66,U55,R34,D71,R55,D58,R83")
(def cdata1 (steps-into-coords (to-vector testdata1)))
(def cdata2 (steps-into-coords (to-vector testdata2)))
(def sectdata (clojure.set/intersection (compute-and-set cdata1) (compute-and-set cdata2)))
(defn compute-intersections
  {:test (fn [] (is (= 610
                       (second (sort (compute-intersections cdata1 cdata2 sectdata))))))}
  [coordlist1 coordlist2 section]
  (second (sort (reduce #(conj %1 (+ (compute-til-stop coordlist1 %2)
                         (compute-til-stop coordlist2 %2))) '() section))))

(defn parse-and-compute
  [text1 text2]
  (let [c1 (compute-and-set (steps-into-coords (to-vector text1)))
        c2 (compute-and-set (steps-into-coords (to-vector text2)))]
    (println (clojure.set/intersection c1 c2))
    (compute-intersections c1 c2 (clojure.set/intersection c1 c2)
    )))
