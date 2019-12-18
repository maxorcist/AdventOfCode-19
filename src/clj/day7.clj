(ns day7
  (:use [day5 :exclude [input output-val compute]]
        [clojure.repl]
        [clojure.test]))

(def input (to-vector (slurp (clojure.java.io/resource "day7"))))
(def testinput (to-vector "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))
(def testinput2 (to-vector "3,23,3,24,1002,24,10,24,1002,23,-1,23,
101,5,23,23,1,24,23,23,4,23,99,0,0"))

(def phases (for [a (range 5)
                  b (range 5)
                  c (range 5)
                  d (range 5)
                  e (range 5)
                  :when (distinct? a b c d e)] [a b c d e]))

(def state (atom 0))

(defn output-val
  [input pos]
  (let [[op param1 &_] (op-params input pos)]
    (do (swap! state + param1)
        input)))

(defn compute
  ([input startvalue] (compute input startvalue 0))
  ([input startvalue pos]
   (loop [input input
          pos pos]
     (condp = (mod (get input pos) 10)
       nil nil
       9 (deref state)
       8 (recur (equal input pos) (+ pos 4))
       7 (recur (less-than input pos) (+ pos 4))
       6 (recur input (jump-if-false input pos))
       5 (recur input (jump-if-true input pos))
       4 (recur (output-val input pos) (+ pos 2))
       3 (recur (input-val input pos startvalue) (+ pos 2))
       2 (recur (multiply-vals input pos) (+ pos 4))
       1 (recur (add-vals input pos) (+ pos 4))))))

(defn pre-process
{:test (fn [] (is (= 43210 (pre-process testinput 0 4321))))} 
  ([input startvalue] (pre-process input startvalue 0))
  ([input startvalue nextvalue]
   (reset! state 0)
   (compute (input-val input 0 startvalue) nextvalue 2)))

(defn iter-amps
  {:test (fn [] (is (= 43210 (iter-amps testinput [4 3 2 1 0]))))}
  [input phases]
  (reset! state 0)
  (loop [output 0
         phases phases]
    (if (empty? phases)
      output
      (recur (pre-process input (first phases) output)
             (rest phases)))))

(defn iter-phases
  [input phases]
  (loop [outputs []
         phases phases]
    (if (empty? phases)
      (last (sort outputs))
      (recur (conj outputs (iter-amps input (first phases)))
             (rest phases)))))

(reset! state 0)

(def testinput-feedback (to-vector "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"))
(def feedback-phases (for [a (range 5 10)
                  b (range 5 10)
                  c (range 5 10)
                  d (range 5 10)
                  e (range 5 10)
                  :when (distinct? a b c d e)] [a b c d e]))
