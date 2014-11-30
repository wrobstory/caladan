(ns caladan.arrays
  (:refer-clojure :exclude [filter take])
  (:require [hiphip.int :as hhi]
            [caladan.agg :refer :all]
            [vertigo.structs :as s]
            [vertigo.core :as v])
  (:import (java.io.Writer)))

(set! *warn-on-reflection* true)

;; Homogeneous Arrays, either of a given type or a categorical array with
;; int32 indices and string levels
(defprotocol Array
  (take [this length])
  (filter [this pred]))

;; Categorical array: primitive integer indices (accepts extremely large cardinality)
;; Levels are a simple Clojure vector
(deftype CategoricalArray [levels ^longs indices]
  Array
  (take [this n]
    "Returns vector for the first n items in the array. Currently raises if
    more than n items in array."
    (let [return-vector (transient [])
          levels (.levels this)
          indices (.indices this)
          idx-length (hhi/alength indices)
          bounded (if (> n idx-length) idx-length n)]
      (hhi/doarr [x indices :range [0 bounded]]
        (conj! return-vector (get levels x)))
      (vec (persistent! return-vector))))

  (filter [this pred]
    "Returns a vector of the items in the array for which (pred item)
    returns true. Pred should be free of side-effects"
    (let [filtered-indices (filter-level-indices pred (.levels this))]
      (subset (.indices this) filtered-indices (.levels this)))))


(defmethod clojure.core/print-method caladan.arrays.CategoricalArray
  [^caladan.arrays.CategoricalArray arr ^java.io.Writer w]
    (let [^longs indices (.indices arr)
          levels (.levels arr)]
      (.write w (clojure.string/join "\n" [(str "Cardinality: " (count levels))
                                           (str "Take 5: " (take arr 5))]))))

(defn make-categorical-array
  "Given a vector, return a caladan CategoricalArray"
  [^clojure.lang.PersistentVector arr-vec]
    (let [levels-indices (get-levels-and-indices arr-vec)]
      (CategoricalArray. (first levels-indices) (second levels-indices))))
