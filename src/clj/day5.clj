(ns day5
  (:use clojure.repl
        clojure.test))

(def input (slurp (clojure.java.io/resource "day5")))

(defn to-vector [input]
  (vec (map read-string (clojure.string/split input #","))))

(def input (to-vector input))

(defn add-op
  {:test (fn [] (is (= [5 2 3 4] (add-op [1 2 3 4] 2 3 0))))}
  [input val1 val2 addr]
  (assoc input addr (+ val1 val2)))
(defn mult-op
  {:test (fn [] (is (= [6 2 3 4] (mult-op [1 2 3 4] 2 3 0))))}
  [input val1 val2 addr]
  (assoc input addr (* val1 val2)))
(defn jump-if-true
  [pos val1 addr1]
  (if (zero? val1)
    pos
    addr1))
(defn jump-if-false
  [pos val1 addr1]
  (if (pos? val1)
    addr1
    pos))
(defn less-than
  [input val1 val2 addr]
  (if (< val1 val2)
    (assoc input addr 1)
    (assoc input addr 0)))
(defn eq
  [input val1 val2 addr]
  (if (= val1 val2)
    (assoc input addr 1)
    (assoc input addr 0)))

(defn pos-mode [input addr] (get input addr))
(defn im-mode [_ value]  value)
(defn get-mode [x] (if (= x 0)
                 pos-mode
                 im-mode))

(defn opcode
  [input]
  (let [input (map #(- (int %) 48) (reverse (format "%05d" input)))
        [op1 op2 param1 param2 param3] input
        op (+ op1 (* op2 10))]
    (condp = op
      01 (fn [in addr1 addr2 addr3]
           (add-op in
                   ((get-mode param1) in addr1)
                   ((get-mode param2) in addr2)
                   addr3))
      02 (fn [in addr1 addr2 addr3]
           (mult-op in
                    ((get-mode param1) in addr1)
                    ((get-mode param2) in addr2)
                    addr3))
      03 (fn [input addr inval] (assoc input addr inval))
      04 (fn [input addr _] (println (get input addr)) input)
      99 println
      #(println "error" %&))))


(defn intcode-comp
  {:test (fn [] (is (= 6 (intcode-comp [01 4 5 0 03 3 99])))
           (is (= 16 (intcode-comp [102 4 1 0 04 0 99])))
           (is (= 3 (intcode-comp [3 9 8 9 10 9 4 9 99 -1 8]))))}
  [input]
  (let [startvalue 1]
    (loop [input input
           pos 0]
      (condp contains? (mod (get input pos) 10)
        #{9 nil} (do (println input) (first input))
        #{8}
        (recur (eq input (get input (+ pos 1)) (get input (+ pos 2)) (get input (+ pos 3)))
         (+ pos 4))
        #{7}
        (recur (less-than input (get input (+ pos 1)) (get input (+ pos 2)) (get input (+ pos 3)))
         (+ pos 4))
        #{6}
        (recur input (jump-if-false pos (get input (+ pos 1)) (get input (+ pos 2))))
        #{5}
        (recur input (jump-if-true pos (get input (+ pos 1)) (get input (+ pos 2))))
        #{3 4}
        (recur ((opcode (get input pos))
                input
                (get input (+ pos 1))
                startvalue)
               (+ pos 2))
        #{1 2}
        (recur ((opcode (get input pos))
                input
                (get input (+ pos 1))
                (get input (+ pos 2))
                (get input (+ pos 3)))
               (+ pos 4))))))
