(ns day6
  (:use clojure.repl
        clojure.test))

(def input (clojure.string/split
            (slurp (clojure.java.io/resource "day6"))
            #"\n"))
(def orbiters (map #(subs % 4) input))
(def orbitmap (reduce #(assoc %1 (subs %2 4) (subs %2 0 3)) {} input))

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
