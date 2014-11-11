(ns caladan.arrays
  (:require [hiphip.long :as hhl])
  (:refer-clojure :exclude [filter])
  (:import (java.util HashSet)))

(defn subset
  [^longs indices hashed-indices levels]
    (let [return-vector (transient [])]
      (hhl/doarr [i indices]
        (if (.contains hashed-indices i)
          (conj! return-vector (get levels i))))
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

(defn get-indexed-indices
  [in-seq levels]
    (let [values (make-array Long/TYPE (count in-seq))]
      (doseq [[i value] (map-indexed vector in-seq)]
        (aset ^longs values i (long (.indexOf levels ^String value))))
      values))

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

(defn make-indexed-array
  [str-seq]
    (let [levels (into [] (apply sorted-set str-seq))
          indices (get-indexed-indices str-seq levels)]
      (IndexedArray. levels indices)))
