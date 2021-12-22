(ns advent-of-code-2021-clj.day20)
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def deterministic-dice (cycle (range 1 101)))

(defn new-pos [old-pos dice-roll]
  (inc (mod (- (+ old-pos (reduce + dice-roll)) 1) 10)))

(defn part1 []
  (loop [player1-pos 4
         player2-pos 10
         player1-score 0
         player2-score 0
         dice deterministic-dice
         num-rolls 0]
    (cond
      (>= player1-score 1000)
      (* player2-score num-rolls)

      (>= player2-score 1000)
      (* player1-score num-rolls)

      :else
      (let [player1-new-pos (new-pos player1-pos (take 3 dice))
            player2-new-pos (new-pos player2-pos (take 3 (drop 3 dice)))
            player1-new-score (+ player1-score player1-new-pos)
            player2-new-score (+ player2-score player2-new-pos)
            new-dice (drop 6 dice)
            new-num-rolls (+ num-rolls 6)]

        (if (>= player1-new-score 1000)
          (* player2-score (+ 3 num-rolls))
          (recur player1-new-pos
                 player2-new-pos
                 player1-new-score
                 player2-new-score
                 new-dice
                 new-num-rolls))))))

(part1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

