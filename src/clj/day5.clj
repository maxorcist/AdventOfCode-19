(ns day5
  (:use clojure.repl
        clojure.test))

(def input (slurp (clojure.java.io/resource "day5")))

(defn to-vector [input]
  (vec (map read-string (clojure.string/split input #","))))

(def input (to-vector input))

(defn opcode-get
  [input pos]
  (let [[op1 op2 p1 p2 p3] (map #(- (int %) 48) (reverse (format "%05d" (get input pos))))
        op (+ (* op2 10) op1)]
    [op p1 p2 p3]))
(defn param-eval
  [input pmode value]
  (if (= pmode 0)
    (get input value)
    value))
(defn op-params
  [input pos]
  (let [[op p1 p2 p3] (opcode-get input pos)
        eval-param1 (param-eval input p1 (get input (+ pos 1)))
        eval-param2 (param-eval input p2 (get input (+ pos 2)))
        eval-param3 (get input (+ pos 3))]
    [op eval-param1 eval-param2 eval-param3]))

(defn add-vals
  [input pos]
  (let [[op param1 param2 addr] (op-params input pos)]
      (assoc input addr (+ param1 param2))))
(defn multiply-vals
  [input pos]
  (let [[op param1 param2 addr] (op-params input pos)]
    (assoc input addr (* param1 param2))))
(defn input-val
  [input pos startvalue]
  (assoc input (get input (+ pos 1)) startvalue))
(defn output-val
  [input pos]
  (let [[op param1 &_] (op-params input pos)]
    (do (println param1)
        input)))
(defn jump-if-true
  [input pos]
  (let [[op param1 param2 &_] (op-params input pos)]
    (if (zero? param1)
      (+ pos 3)
      param2)))
(defn jump-if-false
  [input pos]
  (let [[op param1 param2 &_] (op-params input pos)]
    (if (zero? param1)
      param2
      (+ pos 3))))
(defn less-than
  [input pos]
  (let [[op param1 param2 addr] (op-params input pos)]
    (if (< param1 param2)
      (assoc input addr 1)
      (assoc input addr 0))))
(defn equal
  [input pos]
  (let [[op param1 param2 addr] (op-params input pos)]
    (if (= param1 param2)
      (assoc input addr 1)
      (assoc input addr 0))))

(defn compute
  ([input] (compute input 5))
  ([input startvalue]
   (loop [input input
          pos 0]
     (condp = (mod (get input pos) 10)
       nil nil
       9 "hej";(do (println (last input)))
       8 (recur (equal input pos) (+ pos 4))
       7 (recur (less-than input pos) (+ pos 4))
       6 (recur input (jump-if-false input pos))
       5 (recur input (jump-if-true input pos))
       4 (recur (output-val input pos) (+ pos 2))
       3 (recur (input-val input pos startvalue) (+ pos 2))
       2 (recur (multiply-vals input pos) (+ pos 4))
       1 (recur (add-vals input pos) (+ pos 4))
       ))))
