(ns day2
  (:use [clojure.repl]))

(def input (slurp (clojure.java.io/resource "day2")))

(defn to-vector [input]
  (vec (map read-string (clojure.string/split input #","))))

(def testdata (to-vector "1,9,10,3,2,3,11,0,99,30,40,50"))

(defn add-op
  {:test #(clojure.test/is (= [15 7 8 9] (add-op [5 7 8 9] 1 2 0)))}
  [data addr1 addr2 addr3]
  (assoc data addr3 (+ (get data addr1) (get data addr2))))
(defn multiply-op
  {:test #(clojure.test/is (= [56 7 8] (multiply-op [5 7 8] 1 2 0)))}
  [data addr1 addr2 addr3]
  (assoc data addr3 (* (get data addr1) (get data addr2))))
(defn inc4 [x] (+ x 4))
(defn compute
  [data]
  (loop [input data
         pos 0]
    (condp = (get input pos)
      1 (recur
         (add-op input
                 (get input (+ pos 1))
                 (get input (+ pos 2))
                 (get input (+ pos 3))) (inc4 pos))
      2 (recur
         (multiply-op input
                      (get input (+ pos 1))
                      (get input (+ pos 2))
                      (get input (+ pos 3))) (inc4 pos))
          input)))

(defn restore [input] (#(assoc % 2 2) (assoc (to-vector input) 1 12)))

(def result-one (first (compute (restore input))))

(defn restore-noun-verb [input noun verb]
  (#(assoc % 2 verb) (assoc (to-vector input) 1 noun)))

(defn noun-verb [noun verb]
  (first (compute (restore-noun-verb input noun verb))))

(for [noun (range 99) verb (range 99)
      :when (= 19690720 (noun-verb noun verb))] (+ (* 100 noun)
