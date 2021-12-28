(ns advent-of-code-2021-clj.day20)
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])

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

(defn new-pos [old-pos dice-roll]
  (inc (mod (- (+ old-pos dice-roll) 1) 10)))

(defn new-state-from-roll [player-pos player-score roll]
  (let [player-new-pos (new-pos player-pos roll)
        player-new-score (+ player-score player-new-pos)]
    [player-new-pos player-new-score roll]))

(def all-possible-rolls (map #(reduce + %) (combo/selections [1 2 3] 3)))

(def mem (atom {:1 0 :2 0}))

(defn dirac [max-score player1-pos player2-pos player1-score player2-score]
  (cond
    (>= player1-score max-score)
    {:1 1 :2 0}

    (>= player2-score max-score)
    {:1 0 :2 1}

    (get-in @mem [player1-pos player2-pos player1-score player2-score])
    (get-in @mem [player1-pos player2-pos player1-score player2-score])

    :else
    (let [player1-new-states (map #(new-state-from-roll player1-pos player1-score %) all-possible-rolls)
          {player1-wins true player1-other-states false} (group-by (fn [[pos score roll]] (>= score max-score)) player1-new-states)
          player1-other-rolls (map last player1-other-states)
          wins {:1 (count player1-wins) :2 0}
          rolls (combo/cartesian-product player1-other-rolls all-possible-rolls)
          new-wins (reduce
                    (fn [w [roll1 roll2]]
                      (let [player1-new-pos (new-pos player1-pos roll1)
                            player2-new-pos (new-pos player2-pos roll2)
                            player1-new-score (+ player1-score player1-new-pos)
                            player2-new-score (+ player2-score player2-new-pos)
                            new-wins (merge-with +' w (dirac max-score player1-new-pos player2-new-pos player1-new-score player2-new-score))]
                        new-wins))
                    wins
                    rolls)]

      (swap! mem #(assoc-in % [player1-pos player2-pos player1-score player2-score] new-wins))
      new-wins)))

(dirac 21 4 10 0 0)