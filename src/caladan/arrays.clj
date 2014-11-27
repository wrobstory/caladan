(ns caladan.arrays
  (:refer-clojure :exclude [filter take])
  (:require [hiphip.long :as hhl]
            [caladan.agg :refer :all]
            [vertigo.structs :as s]
            [vertigo.core :as v]))

(set! *warn-on-reflection* true)

;; Homogeneous Arrays, either of a given type or a categorical array with
;; int32 indices and string levels
(defprotocol Array
  (take [this length])
  (filter [this pred]))

;; Categorical array: vertigo buffer of int32 indices (accepts extremely large cardinality)
;; Levels are a simple Clojure vector
(deftype CategoricalArray [levels ^:s/int32 indices]
  Array
  (take [this n]
    "Returns vector for the first n items in the array. Currently raises if
    more than n items in array."
    (let [^:s/int32 indices (.indices this)
          return-vector (transient [])
          levels (.levels this)]
      (v/doreduce [x indices :limit n] [container return-vector]
        (conj! container (get levels x))
         container)
      (vec (persistent! return-vector))))

  (filter [this pred]
    "Returns a vector of the items in the array for which (pred item)
    returns true. Pred should be free of side-effects"
    (let [filtered-indices (filter-level-indices pred (.levels this))]
      (subset (.indices this) filtered-indices (.levels this)))))

(defn make-categorical-array
  "Given a vector, return a caladan CategoricalArray"
  [^clojure.lang.PersistentVector arr-vec]
    (let [levels-indices (get-levels-and-indices arr-vec)]
      (CategoricalArray. (first levels-indices) (second levels-indices))))
