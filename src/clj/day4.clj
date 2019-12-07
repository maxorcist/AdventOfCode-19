(ns day4
  (:use clojure.repl
        clojure.test))

(def hej [1 2 3 3 4])
(def from-digit 193651)
(def to-digit 649729)
(def testdata-coll [111111 223450 123789 112233 123444 111122])

(defn to-vector [x]
  (map #(- (int %) 48) (str x)))

(defn has-double [input]
  (let [[x y] input]
    (if (not (nil? y))
     (if (= x y)
       (vector x y)
       (recur (rest input)))
     false)))

(defn rising? [input]
  (= input (sort input)))

(def first-result
  (for [x (range from-digit to-digit)
        :when (and (has-double (to-vector x))
                   (rising? (to-vector x)))] x))

;; part 2 (reverse and check for no more than 2)

(defn reverse-to-vector
  {:test #(is (= [5 4 3 2 1] (reverse-to-vector 12345)))}
  [x]
  (reverse (map #(- (int %) 48) (str x))))

(defn has-just-double
  {:test (fn [] (is (has-just-double [1 1 2 2 3 3]))
           (is (not (has-just-double [1 2 3 4 4 4])))
           (is (has-just-double [1 1 1 1 2 2])))}
  [input]
  (let [[x y z] input]
    (if (nil? y)
      false
      (if (= x y)
        (if (= x y z)
         (recur (filter #(not (= x %)) input))
         [x y])
       (recur (rest input))))))

(def second-result
  (for [x (range from-digit to-digit)
        :when (and (rising? (to-vector x))
                   (has-just-double (to-vector x)))] x))

(defn second-runner [from-digit to-digit]
  (for [x (range from-digit to-digit)
        :when (and (rising? (to-vector x))
                   (has-just-double (to-vector x)))] x))

