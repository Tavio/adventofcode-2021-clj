(ns advent-of-code-2021-clj.day19)
(require '[clojure.java.io :as io])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.string :as str])
(require '[clojure.set :as sets])
(require '[advent-of-code-2021-clj.day19input :as day19input])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn relative-to-origin
  "Takes a scanner and an origin, and returns a pair where the first element is the origin, and the value
   is the scanner's beacons mapped to their coordinates relative to the origin."
  [scanner [x y z]]
  [(keyword (str x "," y "," z)) (set (map (fn [[beacon-x beacon-y beacon-z]]
                                             [(- beacon-x x) (- beacon-y y) (- beacon-z z)])
                                           scanner))])

(defn find-common-origin-beacon
  "Finds the first pair of beacons (b1, b2) such that, when all beacons from scanner0 are expressed in relation
   to origin b1, and all beacons in scanner1 are expressed in relation to origin b2, the resulting sets of coordinates
   have an intersection of size at least 12. Returns nil if no such pair exists."
  [scanner0 scanner1]
  (if-let [[[b1 _] [b2 _]] (first (filter (fn [[[_ beacons1] [_ beacons2]]]
                                            (>= (count (sets/intersection beacons1 beacons2)) 12))
                                          (combo/cartesian-product
                                           (map #(relative-to-origin scanner0 %) scanner0)
                                           (map #(relative-to-origin scanner1 %) scanner1))))]
    (let [[x1 y1 z1] (map #(Integer/parseInt %) (str/split (name b1) #","))
          [x2 y2 z2] (map #(Integer/parseInt %) (str/split (name b2) #","))]
      [[x1 y1 z1] [x2 y2 z2]])
    nil))

(defn translate-scanner
  "Takes the coordinates (x,y,z) of a scanner and translates that scanner's beacons' coordinates relative
   to a global origin."
  [scanner x y z]
  (map (fn [[beacon-x beacon-y beacon-z]] [(+ x beacon-x) (+ y beacon-y) (+ z beacon-z)]) scanner))



(defn turn-right [[x y z]] [(- z) y x])
(defn turn-right-n [n [x y z]]
  ((apply comp (repeat n turn-right)) [x y z]))

(defn look-up [[x y z]]
  [x (- z) y])
(defn look-up-n
  [n [x y z]]
  ((apply comp (repeat n look-up)) [x y z]))

(defn roll-right [[x y z]]
  [(- y) x z])
(defn roll-right-n
  [n [x y z]]
  ((apply comp (repeat n roll-right)) [x y z]))

(def orientations
  (for [turn [identity turn-right (partial turn-right-n 2) (partial turn-right-n 3) look-up (partial look-up-n 3)]
        roll [identity roll-right (partial roll-right-n 2) (partial roll-right-n 3)]]
    (apply comp [turn roll])))

(defn merge-scanners
  "Tries to merge two scanners if they are found to be next to one another.
  If the merge is successful, returns true, the position of other-scanner relative to scanner, and the merged scanner.
  Otherwise, returns false and other-scanner unchanged."
  [scanner other-scanner]
  (if-let [[[[x1 y1 z1] [x2 y2 z2]] oriented-beacons] (->> (map #(vector % other-scanner) orientations)
                                                           (map (fn [[orientation beacons]] (map orientation beacons)))
                                                           (map (fn [s] [(find-common-origin-beacon scanner s) s]))
                                                           (filter #(not (nil? (first %))))
                                                           (first))]
    (let [[other-scanner-x other-scanner-y other-scanner-z] [(- x1 x2) (- y1 y2) (- z1 z2)]
          other-scanner-translated (translate-scanner oriented-beacons other-scanner-x other-scanner-y other-scanner-z)]
      [true [other-scanner-x other-scanner-y other-scanner-z] (sets/union (set scanner) (set other-scanner-translated))])
    [false other-scanner]))

(defn part1
  "Loop through the scanners, and at each iteration try to merge the first scanner to all other scanners.
   The first scanner grows each time, and we return it when it's the only one left."
  []
  (loop [scanners day19input/scanners]
    (println (count scanners))
    (if (= (count scanners) 1)
      (count (first scanners))
      (let [[first-scanner & other-scanners] scanners
            pairs (map #(vec [first-scanner %]) other-scanners)
            merged-scanners (map (fn [[scanner other-scanner]] (merge-scanners scanner other-scanner)) pairs)
            {merged true failed false} (group-by first merged-scanners)]
        (recur (concat [(apply sets/union (map last merged))] (map last failed)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn manhattan-distance [[x y z] [xi yi zi]]
  (+ (Math/abs (- xi x)) (Math/abs (- yi y)) (Math/abs (- zi z))))

(defn calculate-scanner-positions []
  (loop [scanners day19input/scanners
         scanner-positions []]
    (println (count scanners))
    (if (= (count scanners) 1)
      scanner-positions
      (let [[first-scanner & other-scanners] scanners
            pairs (map #(vec [first-scanner %]) other-scanners)
            merged-scanners (map (fn [[scanner other-scanner]] (merge-scanners scanner other-scanner)) pairs)
            {merged true failed false} (group-by first merged-scanners)]
        (recur (concat [(apply sets/union (map last merged))] (map last failed))
               (concat scanner-positions (map #(get % 1) merged)))))))

(defn part2
  []
  (let [scanner-pos (calculate-scanner-positions)
        pairs (combo/selections scanner-pos 2)
        distances (map (fn [[scanner other-scanner]] (manhattan-distance scanner other-scanner)) pairs)]
    (apply max distances)))