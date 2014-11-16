(ns caladan.arrays
  (:require [hiphip.long :as hhl])
  (:refer-clojure :exclude [filter])
  (:import (java.util HashSet HashMap)))

(defn subset
  "Given a long-array of indices, a hash map on which to filter the indices, and
  a HashMap of levels that can be numerically indexed, return a subsetted
  vector of values.

  Ex: => (subset [0 1 2 0] (HashSet. [0 1]) [\"foo\" \"bar\" \"baz\"])
      [\"foo\" \"bar\" \"foo\"]"
  [^longs indices hashed-indices levels]
    (let [return-vector (transient [])]
      (hhl/doarr [i indices]
        (if (.contains hashed-indices i)
          (conj! return-vector (nth levels i))))
      (persistent! return-vector)))

(defn filter-level-indices
  [pred levels]
    (let [hashed-levels (HashSet.)
          level-filter (fn [hl item]
                          (if (pred (get item 1))
                            (do
                              (.add hashed-levels (get item 0))
                              hashed-levels)
                            hashed-levels))]
      (reduce level-filter hashed-levels (map-indexed vector levels))
      hashed-levels))

(defprotocol Array
  (takes [this length])
  (filter [this pred]))

(defrecord IndexedArray [levels ^longs indices]
  Array
  (takes [this length]
    (let [return-vector (transient [])
          levels (:levels this)
          indices (:indices this)]
      (hhl/doarr [x indices]
        (conj! return-vector (get levels x)))
      (vec (persistent! return-vector))))
  (filter [this pred]
    (let [filtered-indices (filter-level-indices pred (:levels this))]
      (subset (:indices this) filtered-indices (:levels this)))))

(defn get-indexed-indices
  [in-seq levels]
    (let [values (make-array Long/TYPE (count in-seq))]
      (doseq [[i value] (map-indexed vector in-seq)]
        (aset ^longs values i (long (.get levels value))))
      values))

(defn get-levels-and-indices
  "Perform a single pass across the vector, building a HashMap of idx:value
  for lookup, a vector of level values, and a long-array of indices"
  [arr-seq]
    (let [level-map (HashMap.)
          levels (atom [])
          level-idx (atom 0)
          indices (make-array Long/TYPE (count arr-seq))]
      ;; Single pass over arr-seq.
      (loop [values arr-seq
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

(defn make-indexed-array
  [arr-seq]
    (let [levels-indices (get-levels-and-indices arr-seq)]
      (IndexedArray. (first levels-indices) (second levels-indices))))
