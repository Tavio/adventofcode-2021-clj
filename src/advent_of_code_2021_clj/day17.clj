(ns advent-of-code-2021-clj.day17)
(require '[clojure.java.io :as io])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-in-range? [x y [start-range-x end-range-x] [start-range-y end-range-y]]
  (and (<= start-range-x x end-range-x) (<= start-range-y y end-range-y)))

(defn decrease-x-velocity [x-velocity]
  (cond
    (> x-velocity 0)
    (dec x-velocity)

    (< x-velocity 0)
    (inc x-velocity)

    :else
    x-velocity))

(defn simulate-trajectory
  "Returns nil if the provided velocity doesn't bring the probe into the target area,
   or the max y reached in the trajectory where the prob hit the target area"
  [x-velocity y-velocity [start-range-x end-range-x] [start-range-y end-range-y]]
  (loop [x 0
         y 0
         max-y 0
         curr-x-velocity x-velocity
         curr-y-velocity y-velocity]
    (cond
      (or (> x end-range-x) (< y start-range-y))
      nil

      (is-in-range? x y [start-range-x end-range-x] [start-range-y end-range-y])
      max-y

      :else
      (let [new-x (+ x curr-x-velocity)
            new-y (+ y curr-y-velocity)]
        (recur new-x new-y (max max-y new-y) (decrease-x-velocity curr-x-velocity) (dec curr-y-velocity))))))


;; Shamelessly brute force to get the answer, even trying values I know won't work but who cares - it's fast enough.
(apply max (remove nil? (for [y (range -1000 1000)
                              x (range 0 100)]
                          (simulate-trajectory x y [29 73] [-248 -194]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Same as part 1, but count instead of getting the max.
(count (remove nil? (for [y (range -1000 1000)
                          x (range 0 100)]
                      (simulate-trajectory x y [29 73] [-248 -194]))))