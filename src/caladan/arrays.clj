(ns caladan.arrays
  (:require [hiphip.int :as hhi]
            [hiphip.long :as hhl]
            [caladan.agg :refer :all])
  (:import (java.io.Writer)
           (org.roaringbitmap RoaringBitmap))
  (:refer-clojure :exclude [vec]))

(set! *warn-on-reflection* true)

;; Homogeneous Arrays, either of a given type or a categorical array with
;; int32 indices and string levels
(defprotocol Array
  (get-vector [this n])
  (select [this pred]))

;; Categorical array: primitive integer indices (accepts extremely large cardinality)
;; Levels are a simple Clojure vector
(deftype CategoricalArray [levels ^ints indices length]
  Array
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
      (clojure.core/vec (persistent! return-vector))))

  (select [this pred]
    "Returns a vector of the items in the array for which (pred item)
    returns true. Pred should be free of side-effects"
    (let [filtered-indices (filter-level-indices pred (.levels this))
          [levels indices] (cat-subset-on-levels (.indices this) filtered-indices (.levels this))]
      (CategoricalArray. levels indices (hhi/alength indices)))))


(defmethod clojure.core/print-method caladan.arrays.CategoricalArray
  [^caladan.arrays.CategoricalArray arr ^java.io.Writer w]
    (let [^longs indices (.indices arr)
          levels (.levels arr)]
      (.write w (clojure.string/join ", " [(str "<Categorical Array>")
                                           (str "Cardinality: " (count levels))
                                           (str "Count: " (.length arr))
                                           (str "Take 5: " (get-vector arr 5))]))))

(defn make-categorical-array
  "Given a vector, return a caladan CategoricalArray"
  [^clojure.lang.PersistentVector arr-vec]
    (let [[levels indices] (get-levels-and-indices arr-vec)]
      (CategoricalArray. levels indices (hhi/alength indices))))

;; Numeric Arrays. primitive values, value indices kept in bit-index.

(deftype IntegerArray [^ints values ^RoaringBitmap val-idx length]
  Array)

(deftype LongArray [^longs values ^RoaringBitmap val-idx length]
  Array)

(deftype FloatArray [^floats values ^RoaringBitmap val-idx length]
  Array)

(defn take-int-arr
  [n ^IntegerArray int-arr]
    (take-num-arr (.val-idx int-arr) (.values int-arr) int-slicer n))

(defn take-long-arr
  [n ^LongArray long-arr]
    (take-num-arr (.val-idx long-arr) (.values long-arr) long-slicer n))

(defn make-integer-array
  "Given a vector, return a caladan IntegerArray"
  [^clojure.lang.PersistentVector arr-vec]
    (let [[values val-idx length] (get-int-arr-comp arr-vec)]
      (IntegerArray. values val-idx length)))

(defn vec
  "Get vector from array"
  [array]
    (get-vector array (.length array)))