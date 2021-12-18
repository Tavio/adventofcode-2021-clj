(ns advent-of-code-2021-clj.day18)
(require '[clojure.java.io :as io])
(require '[clojure.math.combinatorics :as combo])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def max-node-id (atom 0))

(defn get-node-id []
  (let [id @max-node-id]
    (swap! max-node-id inc)
    (keyword (str id))))

(defn to-binary-tree
  "Transforms a list of nested pairs into a binary tree representation.
   Returns the tree and the id of its root."
  ([pairs] (to-binary-tree {} pairs))
  ([result pairs]
   (if (int? pairs)
     (let [node-id (get-node-id)]
       [(assoc result node-id {:value pairs :left nil :right nil :parent nil}) node-id])
     (let [node-id (get-node-id)
           [result-left left-id] (to-binary-tree result (get pairs 0))
           [result-right right-id] (to-binary-tree result (get pairs 1))
           next-result (assoc-in (assoc-in (merge result-left result-right) [left-id :parent] node-id) [right-id :parent] node-id)]
       [(assoc next-result node-id {:left left-id :right right-id :parent nil}) node-id]))))

(defn print-tree
  [tree node]
  (cond
    (nil? node)
    nil

    (get-in tree [node :value])
    (print (get-in tree [node :value]))

    :else
    (do
      (print "[")
      (print-tree tree (get-in tree [node :left]))
      (print ",")
      (print-tree tree (get-in tree [node :right]))
      (print "]"))))

(defn add
  "Adds two snailfish numbers by joining their tree representations to a same, new parent node.
   Returns the merged tree and the id of the new root."
  [left-tree left-root right-tree right-root]
  (let [result (merge left-tree right-tree)
        parent-id (get-node-id)
        parent {:left left-root :right right-root :parent nil}]
    [(assoc-in (assoc-in (assoc result parent-id parent) [left-root :parent] parent-id) [right-root :parent] parent-id) parent-id]))

(defn find-pair-to-explode
  "Returns the leftmost pair that is fours level deep into the tree if it exists. Otherwise, returns nil"
  ([tree tree-root]
   (find-pair-to-explode tree tree-root 0))
  ([tree curr-node depth]
   (cond
     (nil? curr-node)
     nil

     (> depth 4)
     nil

     (and (= 4 depth) (not (get-in tree [curr-node :value])))
     curr-node

     :else
     (if-let [pair (find-pair-to-explode tree (get-in tree [curr-node :left]) (inc depth))]
       pair
       (if-let [pair (find-pair-to-explode tree (get-in tree [curr-node :right]) (inc depth))]
         pair
         nil)))))

(defn leftmost-descendant
  "Returns the leftmost leaf node that is a descendant of node"
  [tree node]
  (cond
    (nil? node)
    nil

    (get-in tree [node :value])
    node

    :else
    (leftmost-descendant tree (get-in tree [node :left]))))

(defn rightmost-descendant
  "Returns the rightmost leaf node that is a descendant of node"
  [tree node]
  (cond
    (nil? node)
    nil

    (get-in tree [node :value])
    node

    :else
    (rightmost-descendant tree (get-in tree [node :right]))))

(defn first-to-the-left
  "Returns the left child of the first ancestor of node that has a left child, or nil if not found"
  [tree node]
  (if (nil? node)
    nil
    (let [parent (get-in tree [node :parent])
          node-is-right-child (= node (get-in tree [parent :right]))]
      (if node-is-right-child
        (get-in tree [parent :left])
        (first-to-the-left tree parent)))))

(defn first-to-the-right
  "Returns the right child of the first ancestor of node that has a right child, or nil if not found"
  [tree node]
  (if (nil? node)
    nil
    (let [parent (get-in tree [node :parent])
          node-is-left-child (= node (get-in tree [parent :left]))]
      (if node-is-left-child
        (get-in tree [parent :right])
        (first-to-the-right tree parent)))))

(defn update-first-to-the-left [tree node first-to-the-left]
  (if first-to-the-left
    (update-in tree [first-to-the-left :value] #(+ % (get-in tree [(get-in tree [node :left]) :value])))
    tree))

(defn update-first-to-the-right [tree node first-to-the-right]
  (if first-to-the-right
    (update-in tree [first-to-the-right :value] #(+ % (get-in tree [(get-in tree [node :right]) :value])))
    tree))

(defn explode [tree node]
  (let [first-to-the-left (rightmost-descendant tree (first-to-the-left tree node))
        first-to-the-right (leftmost-descendant tree (first-to-the-right tree node))
        parent (get-in tree [node :parent])]
    (assoc (update-first-to-the-left (update-first-to-the-right tree node first-to-the-right) node first-to-the-left)
           node {:value 0 :parent parent :left nil :right nil})))

(defn find-number-to-split
  "Returns the leftmost number that is 10 or greater. Otherwise, returns nil"
  ([tree curr-node]
   (cond
     (nil? curr-node)
     nil

     (and (get-in tree [curr-node :value]) (>= (get-in tree [curr-node :value]) 10))
     curr-node

     :else
     (if-let [num (find-number-to-split tree (get-in tree [curr-node :left]))]
       num
       (if-let [num (find-number-to-split tree (get-in tree [curr-node :right]))]
         num
         nil)))))

(defn split [tree node]
  (let [value (get-in tree [node :value])
        left-id (get-node-id)
        left {:value (quot value 2) :parent node :left nil :right nil}
        right-id (get-node-id)
        right {:value (int (Math/ceil (/ value 2))) :parent node :left nil :right nil}]
    (update
     (assoc-in (assoc-in (assoc (assoc tree left-id left) right-id right) [node :left] left-id) [node :right] right-id)
     node
     dissoc
     :value)))

(defn reduce-tree
  "Reduces the tree, exploding and splitting numbers until it's not possible anymore.
   Returns the tree and its root."
  [tree root]
  (if-let [to-explode (find-pair-to-explode tree root)]
    (reduce-tree (explode tree to-explode) root)
    (if-let [to-split (find-number-to-split tree root)]
      (reduce-tree (split tree to-split) root)
      [tree root])))

(defn magnitude [tree curr-node]
  (if-let [value (get-in tree [curr-node :value])]
    value
    (let [left-value (magnitude tree (get-in tree [curr-node :left]))
          right-value (magnitude tree (get-in tree [curr-node :right]))]
      (+ (* 3 left-value) (* 2 right-value)))))

(defn reduce-pair [[tree1 root1] [tree2 root2]]
  (let [[merged merged-root] (add tree1 root1 tree2 root2)]
    (reduce-tree merged merged-root)))

(defn part1 []
  (with-open [rdr (io/reader (str (io/resource "2021/day18.txt")))]
    (->> (line-seq rdr)
         (map read-string)
         (map to-binary-tree)
         (reduce reduce-pair)
         (apply magnitude))))


(println part1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn part2 []
  (with-open [rdr (io/reader (str (io/resource "2021/day18.txt")))]
    (let [numbers (->> (line-seq rdr)
                       (map read-string)
                       (map to-binary-tree))
          combinations (combo/combinations numbers 2)
          reduced (map #(apply reduce-pair %) combinations)
          sums (map #(apply magnitude %) reduced)]
      (apply max sums))))


(println part2)