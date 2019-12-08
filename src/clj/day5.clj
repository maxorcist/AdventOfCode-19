(ns day5
  (:use clojure.repl
        clojure.test))

(def input (slurp (clojure.java.io/resource "day5")))

(defn to-vector [input]
  (vec (map read-string (clojure.string/split input #","))))

(defn add-op
  {:test (fn [] (is (= [5 2 3 4] (add-op [1 2 3 4] 2 3 0))))}
  [input val1 val2 addr]
  (assoc input addr (+ val1 val2)))
(defn mult-op [])

(defn pos-mode [x] #())
(defn im-mode [x] x)

(defn opcode
  {:test (fn [] (is (= [02 1 2 3] (opcode 32102))))}
  [input]
  (let [input (map #(- (int %) 48) (reverse (format "%05d" input)))
        [op1 op2 param1 param2 param3] input
        op (+ op1 (* op2 10))]
    [op param1 param2 param3]))

