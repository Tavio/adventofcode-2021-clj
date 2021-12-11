(ns advent-of-code-2021-clj.day10)
(require '[clojure.java.io :as io])

;; Part 1:

(def scores {(keyword ")") 3
             (keyword "]") 57
             (keyword "}") 1197
             (keyword ">") 25137})

(def complements {(keyword ")") \(
                  (keyword "]") \[
                  (keyword "}") \{
                  (keyword ">") \<
                  (keyword "(") \)
                  (keyword "[") \]
                  (keyword "{") \}
                  (keyword "<") \>})

(def openingCharacters (set [\( \[ \{ \<]))

(defn findCorruptedCharacter
  [line]
  (loop [remainingLine line
         stack ""]
    (if (= 0 (count remainingLine))
      nil
      (let [[c & restLine] remainingLine
            [s & restStack] stack]
        (if (contains? openingCharacters c)
          (recur restLine (cons c stack))
          (if (or
               (= 0 (count stack))
               (not (= ((keyword (str c)) complements) s)))
            c
            (recur restLine restStack)))))))

(def part1
  (with-open [rdr (io/reader (str (io/resource "day10.txt")))]
    (->> (line-seq rdr)
         (map findCorruptedCharacter)
         (remove nil?)
         (map #((keyword (str %)) scores))
         (reduce +))))

(println part1)

;; Part 2:

(def complementScores {(keyword ")") 1
                       (keyword "]") 2
                       (keyword "}") 3
                       (keyword ">") 4})

(defn getComplement
  [line]
  (->>
   (reduce (fn [stack c]
             (if (contains? openingCharacters c)
               (cons c stack)
               (let [s (first stack)
                     restStack (rest stack)]
                 (if (or
                      (= 0 (count stack))
                      (not (= ((keyword (str c)) complements) s)))
                   (throw (Exception. "Parse exception: corrupted line"))
                   restStack))))
           []
           line)
   (map #((keyword (str %)) complements))))

(defn getComplementScore
  [complement]
  (reduce (fn [total c]
            (+ (* total 5) ((keyword (str c)) complementScores)))
          0
          complement))

(def part2
  (with-open [rdr (io/reader (str (io/resource "day10.txt")))]
    (let [sortedScores (->> (line-seq rdr)
                            (map #(try (getComplement %) (catch Exception e "")))
                            (filter #(not (empty? %)))
                            (map getComplementScore)
                            (sort))]
      (nth sortedScores (/ (count sortedScores) 2)))))

(println part2)