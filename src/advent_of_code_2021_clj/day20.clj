(ns advent-of-code-2021-clj.day20)
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn print-image
  "For debug purposes only, ignore"
  [image]
  (let [image-height (count (keys image))
        image-width (count (keys (get image 0)))]
    (println (str/join "\n" (map #(apply str %)
                                 (partition image-height
                                            (map #(str/join #" " (str %))
                                                 (for [i (range 0  image-height)
                                                       j (range 0  image-width)]
                                                   (if (= (get-in image [i j]) 1) \# \.)))))))
    (println)))

(defn pairs-to-map
  "Transform a list of pairs into a map where the keys are the first element of each pair,
   and their values are the second element of each pair"
  [pairs]
  (reduce conj {} pairs))

(defn to-binary [line]
  (map-indexed vector (map #(if (= % \#) 1 0) line)))

(defn parse-image []
  (with-open [rdr (io/reader (str (io/resource "2021/day20-image.txt")))]
    (->> (line-seq rdr)
         (map to-binary)
         (map pairs-to-map)
         (map-indexed vector)
         (reduce conj {}))))

(defn parse-algo []
  (with-open [rdr (io/reader (str (io/resource "2021/day20-algo.txt")))]
    (->> (line-seq rdr)
         (first)
         (to-binary)
         (reduce conj {}))))

(defn square-to-pixel
  "Transforms the 3x3 square around a pixel with coordinates (i,j) in image into a pixel using
   the provided algorithm. background is the value that should be used for coordinates (i,j) not
   present in the image."
  [i j image algo background]
  (let [index (Integer/parseInt (apply str [(or (get-in image [(dec i) (dec j)]) background)
                                            (or (get-in image [(dec i) j]) background)
                                            (or (get-in image [(dec i) (inc j)]) background)
                                            (or (get-in image [i (dec j)]) background)
                                            (or (get-in image [i  j]) background)
                                            (or (get-in image [i (inc j)]) background)
                                            (or (get-in image [(inc i) (dec j)]) background)
                                            (or (get-in image [(inc i) j]) background)
                                            (or (get-in image [(inc i) (inc j)]) background)])
                                2)]
    (get algo index)))

(defn enhance-pixel
  [i j image enhanced-image algo background]
  (let [enhanced-pixel (square-to-pixel i j image algo background)]
    (assoc-in enhanced-image [(+ 2 i) (+ 2 j)] enhanced-pixel)))

(defn enhance-image [algo background image]
  (let [image-height (count (keys image))
        image-width (count (keys (get image 0)))
        limit-y (+ 1 image-height)
        limit-x (+ 1 image-width)]
    (loop [i (- 2)
           j (- 2)
           enhanced-image {}]
      (cond
        (= i limit-y)
        enhanced-image

        :else
        (let [new-i (if (= j limit-x) (inc i) i)
              new-j (if (= j limit-x) (- 2) (inc j))]
          (recur new-i
                 new-j
                 (enhance-pixel i j image enhanced-image algo background)))))))

(defn enhance [n]
  (loop [image (parse-image)
         algo (parse-algo)
         background 0
         c 0]
    (cond
      (= c n)
      (count (filter #(= 1 %) (mapcat vals (vals image))))

      :else
      (recur (enhance-image algo background image)
             algo
             (if (= 1 background) 0 1)
             (inc c)))))

(defn part1 []
  (enhance 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn part2 []
  (enhance 50))