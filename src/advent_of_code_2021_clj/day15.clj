(ns advent-of-code-2021-clj.day15)
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])
(require '[clojure.data.priority-map :refer [priority-map]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn getNeighbours [matrix i j]
  (into {} (remove #(nil? (second %))
                   [[(keyword (str (- i 1) "-" j)) (get-in matrix [(- i 1) j])]
                    [(keyword (str (+ i 1) "-" j)) (get-in matrix [(+ i 1) j])]
                    [(keyword (str i "-" (- j 1))) (get-in matrix [i (- j 1)])]
                    [(keyword (str i "-" (+ j 1))) (get-in matrix [i (+ j 1)])]])))

(defn buildAdjacencyMap [matrix]
  (loop [limitY (- (count matrix) 1)
         limitX (- (count (first matrix)) 1)
         i 0
         j 0
         m {}]
    (if (> i limitY)
      m
      (let [newI (if (= j limitX) (inc i) i)
            newJ (if (= j limitX) 0 (inc j))
            neighbours (getNeighbours matrix i j)
            k (keyword (str i "-" j))]
        (recur limitY limitX newI newJ (into m {k neighbours}))))))

(def ^:private inf Double/POSITIVE_INFINITY)

(defn update-costs
  "Returns costs updated with any shorter paths found to curr's unvisisted
  neighbors by using curr's shortest path"
  [g costs costsHash curr]
  (let [curr-cost (get costs curr)]
    (reduce-kv
     (fn [[c cH] nbr nbr-cost]
       (if (contains? costsHash nbr)
         [(update-in c [nbr] min (+ curr-cost nbr-cost))
          (update-in cH [nbr] min (+ curr-cost nbr-cost))]
         [c cH]))
     [costs costsHash]
     (get g curr))))

(defn dijkstra
  "Returns a map of nodes to minimum cost from src using Dijkstra algorithm.
  Graph is a map of nodes to map of neighboring nodes and associated cost.
  Optionally, specify destination node to return once cost is known"
  ([g src]
   (dijkstra g src nil))
  ([g src dst]
   (loop [costs (assoc (zipmap (keys g) (repeat inf)) src 0)
          costsHash (pop (reduce (fn [m [k v]] (assoc m k v)) (priority-map) costs))
          curr src]
     (cond
       (= curr dst)
       (select-keys costs [dst])

       (or (empty? costsHash) (= inf (get costs curr)))
       costs

       :else
       (let [[next-costs next-costs-hash] (update-costs g costs costsHash curr)
             next-node (first (first next-costs-hash))]
         (recur next-costs (pop next-costs-hash) next-node))))))

(defn into-2d-vec
  [cast-fn matrix]
  (vec
   (map (fn [array]
          (vec (map cast-fn array)))
        matrix)))

(defn part1 []
  (with-open [rdr (io/reader (str (io/resource "2021/day15.txt")))]
    (let [lines (map #(str/split % #"") (line-seq rdr))
          matrix (into-2d-vec #(Integer/parseInt %) lines)
          adjacencyMap (buildAdjacencyMap matrix)
          start :0-0
          end (keyword (str (- (count matrix) 1) "-" (- (count (first matrix)) 1)))]
      (println (dijkstra adjacencyMap start end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn incrementElement [x i]
  (inc (mod (- (+ x i) 1) 9)))

(defn expandHorizontally [matrix n]
  (map #(concat % (flatten (for [i (range 1 n)]
                             (map (fn [n] (incrementElement n i)) %)))) matrix))

(defn expandVertically [matrix n]
  (concat matrix (apply interleave (map #(for [i (range 1 n)]
                                           (map (fn [n] (incrementElement n i)) %))
                                        matrix))))
(defn expand [matrix n]
  (expandHorizontally (expandVertically matrix n) n))

(defn part2 []
  (with-open [rdr (io/reader (str (io/resource "2021/day15.txt")))]
    (let [lines (map #(str/split % #"") (line-seq rdr))
          matrix (into-2d-vec #(Integer/parseInt %) lines)
          expandedMatrix (into-2d-vec identity (expand matrix 5))
          adjacencyMap (buildAdjacencyMap expandedMatrix)
          start :0-0
          end (keyword (str (- (count expandedMatrix) 1) "-" (- (count (first expandedMatrix)) 1)))]
      (time (dijkstra adjacencyMap start end)))))