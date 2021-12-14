(ns advent-of-code-2021-clj.day12)
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(defn parseCaves
  [lines]
  (reduce (fn
            [caves line]
            (let [[from to] (str/split line #"-")
                  kFrom (keyword from)
                  kTo (keyword to)
                  fromNeighbours (or (kFrom caves) [])
                  toNeighbours (or (kTo caves) [])]
              (into caves {kFrom (cons kTo fromNeighbours)
                           kTo (cons kFrom toNeighbours)})))
          {} lines))

(defn isBig
  [cave]
  (Character/isUpperCase (first (drop 1 (str cave)))))

(defn findPaths
  [currentPath currentCave caves]
  (if (= currentCave :end)
    [currentPath]
    (let [neighbours (currentCave caves)]
      (reduce (fn [nextPaths neighbour]
                (if (or (isBig neighbour) (not (some #{neighbour} currentPath)))
                  (concat (findPaths (conj currentPath neighbour) neighbour caves) nextPaths)
                  nextPaths))
              [] neighbours))))

(def part1
  (with-open [rdr (io/reader (str (io/resource "day12.txt")))]
    (->> (line-seq rdr)
         (parseCaves)
         (findPaths [:start] :start)
         (count)
         (println))))

(defn notStartNorEnd
  [cave]
  (not (or (= :start cave) (= :end cave))))

(defn findPathsPart2
  [currentPath currentCave hasRepeated cache caves]
  (if (= currentCave :end)
    1
    (let [neighbours (currentCave caves)]
      (reduce (fn [nextPaths neighbour]
                (if (or (isBig neighbour) (not (some #{neighbour} currentPath)))
                  (+ (findPathsPart2 (cons neighbour currentPath) neighbour hasRepeated cache caves) nextPaths)
                  (if (and (some #{neighbour} currentPath)
                           (not (isBig neighbour))
                           (not hasRepeated)
                           (notStartNorEnd neighbour))
                    (+ (findPathsPart2 (cons neighbour currentPath) neighbour true cache caves) nextPaths)
                    nextPaths)))
              0 neighbours))))

(def part2
  (with-open [rdr (io/reader (str (io/resource "day12.txt")))]
    (->> (line-seq rdr)
         (parseCaves)
         (findPathsPart2 '(:start) :start false {})
         (println))))