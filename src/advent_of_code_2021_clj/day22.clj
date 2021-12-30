(ns advent-of-code-2021-clj.day22)
(require '[clojure.java.io :as io])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-cubes [x-range y-range z-range state cubes]
  (reduce (fn [c [x y z]]
            (assoc-in c [x y z] state))
          cubes
          (for [x x-range
                y y-range
                z z-range]
            [x y z])))

(def initialization-regex #"(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)")

(defn is-in-initialization-region? [[_ x-range y-range z-range]]
  (and (>= (first x-range) -50) (<= (last x-range) 51)
       (>= (first y-range) -50) (<= (last y-range) 51)
       (>= (first z-range) -50) (<= (last z-range) 51)))

(defn count-cubes [cubes]
  (count (filter #(= 1 %) (for [x (range -50 51)
                                y (range -50 51)
                                z (range -50 51)]
                            (get-in cubes [x y z])))))

(defn part1 []
  (with-open [rdr (io/reader (str (io/resource "2021/day22.txt")))]
    (->> (line-seq rdr)
         (map #(re-matches initialization-regex %))
         (map (fn [[_ state x-start x-end y-start y-end z-start z-end]]
                [(if (= state "on") 1 0)
                 (range (Integer/parseInt x-start) (inc (Integer/parseInt x-end)))
                 (range (Integer/parseInt y-start) (inc (Integer/parseInt y-end)))
                 (range (Integer/parseInt z-start) (inc (Integer/parseInt z-end)))]))
         (filter is-in-initialization-region?)
         (reduce (fn [cubes [state x-range y-range z-range]]
                   (set-cubes x-range y-range z-range state cubes))
                 {})
         (count-cubes))))

(part1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn range-intersection [range1 range2]
  (cond
    (and (>= (last range2) (first range1)) (<= (last range2) (last range1)))
    [(max (first range1) (first range2)) (last range2)]

    (and (>= (first range2) (first range1)) (<= (first range2) (last range1)))
    [(first range2) (min (last range1) (last range2))]

    (and (<= (first range2) (first range1)) (>= (last range2) (last range1)))
    range1

    :else
    nil))

(defn calculate-intersection [[x-range1 y-range1 z-range1] [x-range2 y-range2 z-range2]]
  (let [x-intersection (range-intersection x-range1 x-range2)
        y-intersection (range-intersection y-range1 y-range2)
        z-intersection (range-intersection z-range1 z-range2)]
    (if (and x-intersection y-intersection z-intersection)
      [x-intersection y-intersection z-intersection]
      nil)))

(defn subtract [[x-range y-range z-range] [x-range2 y-range2 z-range2]]
  (remove (fn [ranges] (some #(> (first %) (last %)) ranges))
          [[x-range [(first y-range) (dec (first y-range2))] z-range]
           [x-range [(inc (last y-range2)) (last y-range)] z-range]

           [[(first x-range) (dec (first x-range2))]  y-range2 z-range]
           [[(inc (last x-range2))  (last x-range)] y-range2 z-range]

           [x-range2 y-range2 [(first z-range) (dec (first z-range2))]]
           [x-range2 y-range2 [(inc (last z-range2)) (last z-range)]]]))

(defn count-cubes [[x-range y-range z-range]]
  (* (inc (- (last x-range) (first x-range)))
     (inc (- (last y-range) (first y-range)))
     (inc (- (last z-range) (first z-range)))))

(defn subtract-instructions [instructions instruction]
  (reduce
   (fn [result next-instruction]
     (if-let [intersection (calculate-intersection next-instruction instruction)]
       (concat (subtract next-instruction intersection) result)
       (cons next-instruction result)))
   []
   instructions))

(defn merge-instructions [instructions]
  (reduce
   (fn [merged-instructions [state x-range y-range z-range]]
     (let [subtracted-instructions (subtract-instructions merged-instructions [x-range y-range z-range])]
       (if (= 1 state)
         (cons [x-range y-range z-range] subtracted-instructions)
         subtracted-instructions)))
   []
   instructions))

(defn part2 []
  (with-open [rdr (io/reader (str (io/resource "2021/day22.txt")))]
    (->> (line-seq rdr)
         (map #(re-matches initialization-regex %))
         (map (fn [[_ state x-start x-end y-start y-end z-start z-end]]
                [(if (= state "on") 1 0)
                 [(Integer/parseInt x-start)  (Integer/parseInt x-end)]
                 [(Integer/parseInt y-start)  (Integer/parseInt y-end)]
                 [(Integer/parseInt z-start)  (Integer/parseInt z-end)]]))
         (merge-instructions)
         (map count-cubes)
         (reduce +))))