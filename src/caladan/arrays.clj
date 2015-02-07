(ns caladan.arrays
  (:require [hiphip.int :as hhi]
            [hiphip.long :as hhl]
            [hiphip.float :as hhf]
            [caladan.agg :refer :all])
  (:import (java.io.Writer)
           (org.roaringbitmap RoaringBitmap))
  (:refer-clojure :exclude [vec count take]))

(set! *warn-on-reflection* true)

;; Homogeneous Arrays, either of a given type or a categorical array with
;; int32 indices and string levels
(defprotocol Array
  (take [this n])
  (get-vector [this n])
  (where [this pred]))

(defprotocol NumericArray
  (sum [this])
  (mean [this])
  (count [this])
  (reduce-where [this pred reducer init])
  (na-count [this]))

;; Categorical array: primitive integer indices (accepts extremely large cardinality)
;; Levels are a simple Clojure vector
(deftype CategoricalArray [levels ^ints indices length]

  Array

  (take [this n]
    "Take n values of the array"
    (let [indices (.indices this)
          levels (.levels this)
          [new-levels new-indices] (take-categorical indices levels n)]
      (CategoricalArray. new-levels new-indices (hhi/alength new-indices))))

  (get-vector [this n]
    "Returns vector for the first n items in the array. Currently raises if
    more than n items in array."
    (let [return-vector (transient [])
          levels (.levels this)
          indices (.indices this)
          idx-length (hhi/alength indices)
          bounded (if (> n idx-length) idx-length n)]
      (hhi/doarr [x indices :range [0 bounded]]
        (conj! return-vector (get levels x)))
      (into [] (persistent! return-vector))))

  (where [this pred]
    "Returns a new categorical array of the items in the array for which (pred item)
    returns true. Pred should be free of side-effects"
    (let [filtered-indices (filter-level-indices pred (.levels this))
          [levels indices] (cat-subset-on-levels (.indices this) filtered-indices (.levels this))]
      (CategoricalArray. levels indices (hhi/alength indices)))))


(defmethod clojure.core/print-method caladan.arrays.CategoricalArray
  [^caladan.arrays.CategoricalArray arr ^java.io.Writer w]
    (let [^longs indices (.indices arr)
          levels (.levels arr)]
      (.write w (clojure.string/join ", " [(str "<Categorical Array>")
                                           (str "Cardinality: " (clojure.core/count levels))
                                           (str "Count: " (.length arr))
                                           (str "Take 5: " (get-vector arr 5))]))))

(defn make-categorical-array
  "Given a vector, return a caladan CategoricalArray"
  [^clojure.lang.PersistentVector arr-vec]
    (let [[levels indices] (get-levels-and-indices arr-vec)]
      (CategoricalArray. levels indices (hhi/alength indices))))

;; Numeric Arrays. primitive values, value indices kept in bit-index.

(deftype IntegerArray [^ints values ^RoaringBitmap val-idx length num-type]

  Array
  NumericArray

  (get-vector [this n] (clojure.core/vec (int-slicer (.values this) n)))

  (take [this n]
    (let [[values val-idx length] (take-num-arr (.values this) (.val-idx this) (.length this) int-slicer n)]
      (IntegerArray. values val-idx length "Integer")))

  (na-count [this]
    (- (.length this) (.getCardinality (.val-idx this))))

  (sum [this]
    (hhi/asum (.values this)))

  (mean [this]
    (hhi/amean (.values this)))

  (where [this pred]
    "Returns a new IntegerArray of the items in the array for which (pred item)
    returns true. Pred should be free of side-effects."
    (let [[values ^RoaringBitmap orig-val-idx ^RoaringBitmap new-val-idx]
          (filter-int-arr (.values this) (.val-idx this) (.length this) pred)]
      (IntegerArray. values new-val-idx (.getCardinality orig-val-idx) "Integer")))

  (reduce-where [this pred reducer init]
    (filter-reduce-int-arr pred reducer init (.values this))))

(deftype LongArray [^longs values ^RoaringBitmap val-idx length num-type]

  Array
  NumericArray

  (get-vector [this n] (clojure.core/vec (long-slicer (.values this) n)))

  (sum [this]
    (hhl/asum (.values this)))

  (mean [this]
    (hhl/amean (.values this))))

(deftype FloatArray [^floats values ^RoaringBitmap val-idx length num-type]

  Array
  NumericArray

  (sum [this]
    (hhf/asum (.values this)))

  (mean [this]
    (hhf/amean (.values this))))

(defmethod clojure.core/print-method caladan.arrays.NumericArray
  [^caladan.arrays.NumericArray arr ^java.io.Writer w]
    (.write w (clojure.string/join ", " [(str "<Numeric Array>")
                                         (str "Type: " (.num-type arr))
                                         (str "Length: " (.length arr))
                                         (str "NA Count: " (na-count arr))])))

(defn make-integer-array
  "Given a vector, return a caladan IntegerArray"
  [^clojure.lang.PersistentVector arr-vec]
    (let [[values val-idx length] (get-int-arr-comp arr-vec)]
      (IntegerArray. values val-idx length "Integer")))

(defn vec
  "Get vector from array"
  [array]
    (get-vector array (.length array)))
