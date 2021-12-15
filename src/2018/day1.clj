(ns advent-of-code-2021-clj.day10)
(require '[clojure.java.io :as io])

;; Part 1:

(defn part1 []
  (with-open [rdr (io/reader (str (io/resource "2018/day1.txt")))]
    (->> (line-seq rdr)
         (map #(Integer/parseInt %))
         (reduce + 0))))

(part1)

;; Part 2:

(defn applyFreqs [freqs, curr, seen]
  (loop [[f & fs] freqs
         c curr
         s seen]
    (if (contains? s c)
      [c, s, true]
      (if (nil? f)
        [c, s, false]
        (recur fs (+ c f) (conj s c))))))

(defn part2 []
  (with-open [rdr (io/reader (str (io/resource "2018/day1.txt")))]
    (loop [freqs (map #(Integer/parseInt %) (line-seq rdr))
           curr 0
           seen #{}
           found false]
      (if found
        curr
        (let [[newCurr, newSeen, newFound] (applyFreqs freqs curr seen)]
          (recur freqs newCurr newSeen newFound))))))

(part2)

