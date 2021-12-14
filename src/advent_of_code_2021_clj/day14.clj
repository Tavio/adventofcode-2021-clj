(ns advent-of-code-2021-clj.day12)
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn expandPolymer
  [template insertionRulesMap]
  (let [pairs (partition 2 1 template)
        insertions (map #((keyword (apply str %)) insertionRulesMap) pairs)
        pairsInsertions (map vector pairs insertions) ;; this is like zip
        expanded (reduce (fn [result pairInsertion]
                           (let [left (first (first pairInsertion))
                                 middle (last pairInsertion)]
                             (str result left middle)))
                         ""
                         pairsInsertions)]
    (str expanded (last template))))

(defn countElements
  [polymer]
  (reduce (fn [counts element]
            (let [k (keyword (str element))
                  value (or (k counts) 0)]
              (into counts {k (inc value)})))
          {}
          polymer))


(defn expandPolymerTimes
  [template insertionRulesMap n]
  (loop [i 0
         limit n
         result template]
    (println (countElements result))
    (if (= i limit)
      result
      (recur (inc i) limit (expandPolymer result insertionRulesMap)))))

(defn parseInsertionRule
  [insertionRuleStr]
  (let [[from to] (map str/trim (str/split insertionRuleStr #"->"))]
    [(keyword from) to]))

(defn run [n]
  (with-open [rdr (io/reader (str (io/resource "day14.txt")))]
    (let [lines (line-seq rdr)
          template (first (take 1 lines))
          insertionRules (map parseInsertionRule (drop 2 lines))
          insertionRulesMap (into {} insertionRules)
          expandedPolymer (expandPolymerTimes template insertionRulesMap n)]
      (countElements expandedPolymer))))

(run 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn charCount
  "Returns the number of occurrences of a char in the template"
  [c template]
  (count (filter #(= % c) template)))

(defn createCharsMap
  [pairsMap template]
  (let [distinctChars (distinct (apply str (map name (keys pairsMap))))]
    (reduce #(into % {(keyword (str %2)) (charCount %2 template)}) {} distinctChars)))

(defn createPairsMap
  [template insertionRulesMap]
  (let [;; Create a map from every possible pair to the value 0
        ks (keys insertionRulesMap)
        pairsMap (reduce #(into % {%2 0}) {} ks)
        pairs (partition 2 1 template)]
    ;; Then for every adjacent pair in the template, add one to their value in the map
    (reduce #(update % (keyword (apply str %2)) inc) pairsMap pairs)))

(defn updateCharsMap
  "Adds chars to the chars map as dictated by the pairs present in pairsMap and the rules in insertionRulesMap"
  [insertionRulesMap charsMap pairsMap]
  (reduce-kv (fn [m k v]
               (update m (keyword (k insertionRulesMap)) #(+ % v)))
             charsMap
             pairsMap))

(defn updatePairsMap
  "For every pair in pairs map, remove all of its occurrences from the map, and replace with as many new pairs
   as dictated by the insertionRulesMap"
  [insertionRulesMap pairsMap]
  (reduce-kv (fn [m k v]
               (if (= v 0)
                 m
                 (let [left (first (name k))
                       middle (k insertionRulesMap)
                       right (last (name k))
                       newPairOne (keyword (str left middle))
                       newPairTwo (keyword (str middle right))]
                   (update (update m newPairOne #(+ % v)) newPairTwo #(+ % v)))))
             (reduce #(into % {%2 0}) pairsMap (keys pairsMap))
             pairsMap))

(defn run2 [n]
  (with-open [rdr (io/reader (str (io/resource "day14.txt")))]
    (let [lines (line-seq rdr)
          template (first (take 1 lines))
          insertionRules (into {} (map parseInsertionRule (drop 2 lines)))
          pairsMap (createPairsMap template insertionRules)
          charsMap (createCharsMap pairsMap template)]
      (loop [i 0
             limit n
             pMap pairsMap
             cMap charsMap]
        (if (= i limit)
          cMap
          (recur (inc i)
                 limit
                 (updatePairsMap insertionRules pMap)
                 (updateCharsMap insertionRules cMap pMap)))))))

(let [sortedCharCount (sort-by val (run2 40))]
  (- (val (last sortedCharCount)) (val (first sortedCharCount))))