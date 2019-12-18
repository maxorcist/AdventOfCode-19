(ns day6
  (:use clojure.repl
        clojure.test))

(def input (clojure.string/split
            (slurp (clojure.java.io/resource "day6"))
            #"\r\n|\n"))
(def orbiters (map #(subs % 4) input))
(defn map-orbits [input] (reduce #(assoc %1 (subs %2 4) (subs %2 0 3)) {} input))
(def orbitmap (map-orbits input))

(defn orbited-by
  [orbiter orbitmap]
  (get orbitmap orbiter))

(defn get-orbits
  [orbitmap orbiter]
  (loop [orbiter orbiter
         orbits '()]
    (if (= "COM"(first orbits))
      orbits
      (recur (orbited-by orbiter orbitmap)
             (conj orbits orbiter)))))
(defn count-orbits
  [orbitmap orbiter]
  (dec (count (get-orbits orbitmap orbiter))))
(defn orbits-from-to
  [from to orbitmap]
  (loop [from from
         orbits '()]
    (if (= to (first orbits))
      orbits
      (recur (orbited-by from orbitmap) (conj orbits from)))))

(def small (get-orbits orbitmap "5HT"))

(defn easy-iter
  [orbitmap orbiters]
  (loop [result 0
         orbiters orbiters]
    (if (empty? orbiters)
      result
      (recur (+ result (count-orbits orbitmap (first orbiters)))
             (rest orbiters)))))

(defn steps-to-santa
  [orbitmap from to]
  (let [from-you (set (get-orbits orbitmap from))
        from-san (set (get-orbits orbitmap to))
        section (clojure.set/intersection from-you from-san)]
    (map #(+ -2 (dec (count (orbits-from-to from % orbitmap)))
            (dec (count (orbits-from-to to % orbitmap)))) section)))
