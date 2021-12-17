(ns advent-of-code-2021-clj.day16)
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare parse-packet)

(def hex-map {:0 "0000"
              :1 "0001"
              :2 "0010"
              :3 "0011"
              :4 "0100"
              :5 "0101"
              :6 "0110"
              :7 "0111"
              :8 "1000"
              :9 "1001"
              :A "1010"
              :B "1011"
              :C "1100"
              :D "1101"
              :E "1110"
              :F "1111"})

(defn hex-to-bin [hex-str]
  (apply str (map #((keyword (str %)) hex-map) hex-str)))

(defn header-type [type-id]
  (case type-id
    4 :literal
    :operator))

(defn header-op [type-id]
  (case type-id
    0 +
    1 *
    2 min
    3 max
    4 nil
    5 #(if (> %1 %2) 1 0)
    6 #(if (< %1 %2) 1 0)
    7 #(if (= %1 %2) 1 0)))

(defn parse-header
  "Parses a header from bin-str and returns the parsed header and the rest of the input string"
  [bin-str]
  (let [version-bin (subs bin-str 0 3)
        type-id-bin (subs bin-str 3 6)
        rest (subs bin-str 6)
        type-id (Integer/parseInt type-id-bin 2)]
    [{:version (Integer/parseInt version-bin 2)
      :type (header-type type-id)
      :op (header-op type-id)} rest]))

(defn parse-literal-packet-value
  "Parses the value of a literal type packet from bin-str and returns the parsed value and the rest of the input string"
  [bin-str]
  (let [[value-bin rest-bin-str] (loop [rest-bin-str bin-str
                                        value ""]
                                   (let [continue (= (subs rest-bin-str 0 1) "1")
                                         next-bits (subs rest-bin-str 1 5)
                                         next-value (str value next-bits)
                                         next-rest-bin-str (subs rest-bin-str 5)]
                                     (if continue
                                       (recur next-rest-bin-str next-value)
                                       [next-value next-rest-bin-str])))]
    [(Long/parseLong value-bin 2) rest-bin-str]))

(defn parse-operator-value-by-length [bin-str]
  (loop [length (Integer/parseInt (subs bin-str 0 15) 2)
         rest-bin-str (subs bin-str 15)
         sub-packets []]
    (if (= length 0)
      [sub-packets rest-bin-str]
      (let [[next-packet next-rest-bin-str] (parse-packet rest-bin-str)
            next-length (- length (- (count rest-bin-str) (count next-rest-bin-str)))]
        (recur next-length next-rest-bin-str (conj sub-packets next-packet))))))


(defn parse-operator-value-by-count [bin-str]
  (loop [c (Integer/parseInt (subs bin-str 0 11) 2)
         rest-bin-str (subs bin-str 11)
         sub-packets []]
    (if (= c 0)
      [sub-packets rest-bin-str]
      (let [[next-packet next-rest-bin-str] (parse-packet rest-bin-str)]
        (recur (dec c) next-rest-bin-str (conj sub-packets next-packet))))))

(defn parse-operator-package-value
  "Parses the value of an operator type packet from bin-str and returns the parsed value and the rest of the input string"
  [bin-str]
  (let [length-type-id (subs bin-str 0 1)]
    (case length-type-id
      "0" (parse-operator-value-by-length (subs bin-str 1))
      "1" (parse-operator-value-by-count (subs bin-str 1))
      (throw (Exception. (str "Unknown length type id " length-type-id))))))

(defn parse-packet-value
  "Parses the value of a packet from bin-str and returns the parsed value and the rest of the input string"
  [type bin-str]
  (case type
    :literal (parse-literal-packet-value bin-str)
    :operator (parse-operator-package-value bin-str)))

(defn parse-packet
  "Parses the next packet from bin-str and returns the parsed packet and the rest of the input string"
  [bin-str]
  (let [[header rest-bin-str] (parse-header bin-str)
        [packet-value next-rest-bin-str] (parse-packet-value (:type header) rest-bin-str)]
    [{:header header :value packet-value} next-rest-bin-str]))

(defn version-sum [packet]
  (case (get-in packet [:header :type])
    :operator (reduce + (get-in packet [:header :version]) (map version-sum (:value packet)))
    (get-in packet [:header :version])))

(defn part1 []
  (with-open [rdr (io/reader (str (io/resource "2021/day16.txt")))]
    (->> (line-seq rdr)
         (first)
         (hex-to-bin)
         (parse-packet)
         (first) ;; drops the remaining 0s at the end of the packet
         (version-sum))))

(part1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn calculate-value [packet]
  (let [op (get-in packet [:header :op])]
    (case op
      nil (:value packet)
      (reduce op (map calculate-value (:value packet))))))

(defn part2 []
  (with-open [rdr (io/reader (str (io/resource "2021/day16.txt")))]
    (->> (line-seq rdr)
         (first)
         (hex-to-bin)
         (parse-packet)
         (first) ;; drops the remaining 0s at the end of the packet
         (calculate-value))))

(part2)