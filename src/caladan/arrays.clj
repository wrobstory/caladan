(ns caladan.arrays
  (:require [hiphip.long :as hhl])
  (:refer-clojure :exclude [filter take])
  (:import (java.util HashSet HashMap)))

(defn subset
  "Given a long-array of indices, a hash map on which to filter the indices, and
  a HashMap of levels that can be numerically indexed, return a subsetted
  vector of values.

  Ex: => (subset [0 1 2 0] (HashSet. [0 1]) [\"foo\" \"bar\" \"baz\"])
      [\"foo\" \"bar\" \"foo\"]"
  [^longs indices ^HashSet hashed-indices ^clojure.lang.PersistentVector levels]
    (let [return-vector (transient [])]
      (hhl/doarr [i indices]
        (if (.contains hashed-indices i)
          (conj! return-vector (nth levels i))))
      (persistent! return-vector)))

(defn filter-level-indices
  "Given a predicate and a set of levels, filter the level values and return
  a HashSet of the level indices

  Ex: => (filter-level-indices #(> % 1) [1 2 3])
      {1 2}"
  [pred ^clojure.lang.PersistentVector levels]
    (let [hashed-levels (HashSet.)
          level-filter (fn [hl item]
                          (if (pred (get item 1))
                            (do
                              (.add hashed-levels (get item 0))
                              hashed-levels)
                            hashed-levels))]
      (reduce level-filter hashed-levels (map-indexed vector levels))
      hashed-levels))

(defn get-levels-and-indices
  "Perform a single pass across the vector, building a HashMap of idx:value
  for lookup, a vector of level values, and a long-array of indices"
  [^clojure.lang.PersistentVector arr-vec]
    (let [level-map (HashMap.)
          levels (atom [])
          level-idx (atom 0)
          indices (make-array Long/TYPE (count arr-vec))]
      ;; Single pass over arr-vec.
      (loop [values arr-vec
             idx 0]
        (when (seq values)
          (let [value (first values)
                map-entry (.get level-map value)]
            (if map-entry
              (aset ^longs indices idx (long map-entry))
              (do
                (swap! levels conj value)
                (.put level-map value @level-idx)
                (aset ^longs indices idx (long @level-idx))
                (swap! level-idx inc))))
          (recur (rest values) (inc idx))))
      [@levels indices]))

(defprotocol Array
  (take [this length])
  (filter [this pred]))

(defrecord IndexedArray [levels ^longs indices]
  Array
  (take [this length]
    (let [return-vector (transient [])
          levels (:levels this)
          indices (:indices this)]
      (hhl/doarr [x indices :range [0 length]]
        (conj! return-vector (get levels x)))
      (vec (persistent! return-vector))))
  (filter [this pred]
    (let [filtered-indices (filter-level-indices pred (:levels this))]
      (subset (:indices this) filtered-indices (:levels this)))))

(defn make-indexed-array
  [^clojure.lang.PersistentVector arr-vec]
    (let [levels-indices (get-levels-and-indices arr-vec)]
      (IndexedArray. (first levels-indices) (second levels-indices))))
