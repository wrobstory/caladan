(ns caladan.arrays
  (:require [hiphip.long :as hhl]))

(defprotocol Array
  (takes [this]))

(defrecord CategoricalArray
  [levels
   ^longs indices])

(defn get-categorical-indices
  [str-seq levels]
    (let [values (make-array Long/TYPE (count str-seq))]
      (doseq [[i value] (map-indexed vector str-seq)]
        (aset ^longs values i (long (.indexOf levels ^String value))))
      values))

(defn make-categorical-array
  [str-seq]
    (let [levels (into [] (apply sorted-set str-seq))
          indices (get-categorical-indices str-seq levels)]
      (CategoricalArray. levels indices)))

(extend-protocol Array
  CategoricalArray
    (takes [this length]
      (let [return-vector (transient [])
            levels (:levels this)
            indices (:indices this)]
        (hhl/doarr [x indices]
          (conj! return-vector (get levels x)))
        (vec (persistent! return-vector)))))
