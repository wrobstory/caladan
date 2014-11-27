(ns caladan.agg
  (:require [hiphip.long :as hhl]
            [vertigo.structs :as s]
            [vertigo.bytes :as b]
            [vertigo.primitives :as p]
            [vertigo.core :as v])
  (:import (java.util HashSet HashMap)
           (org.roaringbitmap RoaringBitmap)))

(set! *warn-on-reflection* true)

(defn group-indices
  "Group a given array of level indices, return a HashMap of
  {level-index: Bitmap of row-indices}"
  [^longs indices]
    (let [^HashMap return-map (HashMap.)]
      (hhl/doarr [[i x] indices]
        (let [^RoaringBitmap map-entry (.get return-map x)]
          (if map-entry
            (.add map-entry i)
            (let [^RoaringBitmap new-bitmap (RoaringBitmap.)]
                (do
                  (.add new-bitmap i)
                  (.put return-map x new-bitmap))))))
      return-map))

(defn subset-and-reindex
  "Given an array of level-indices, an array of row indices, and levels, take
  given rows and return new level-indices and levels"
  [^longs level-indices ^longs row-indices ^clojure.lang.PersistentVector levels]
    (let [return-indices (make-array Long/TYPE (count row-indices))
          return-levels (atom [])
          ^HashMap new-level-map (HashMap.)
          level-idx (atom 0)]
      (hhl/doarr [[i x] row-indices]
        (let [row-value (aget ^longs level-indices x)
              level-value (get levels row-value)
              map-entry (.get new-level-map level-value)
              long-level-idx (long @level-idx)]
          (if-not map-entry
            (do
              (.put new-level-map level-value long-level-idx)
              (aset ^longs return-indices i long-level-idx)
              (swap! return-levels conj level-value)
              (swap! level-idx inc))
            (aset ^longs return-indices i (long map-entry)))))
      [@return-levels return-indices]))

(defn subset
  "Given a byte-seq of level indices, a HashSet of hashed-indices, and the level vector
  , return a vector of subsetted values.

  Ex: => (subset (0 1 2 0 1) #{1 2} [foo bar baz])
      [bar baz bar]
      => (subset (0 1 2 0 1) #{0} [foo bar baz])
      [foo foo]"
  [^:s/int32 byte-seq
   ^HashSet hashed-indices
   ^clojure.lang.PersistentVector levels]
    (let [return-vector (transient [])]
      (v/doreduce [x byte-seq] [container return-vector]
        (if (.contains hashed-indices x)
          (do
            (conj! container (nth levels x))
            container)
          container))
      (persistent! return-vector)))

(defn filter-level-indices
  "Given a predicate and a set of levels, filter the level values and return
  a HashSet of the level indices

  Ex: => (filter-level-indices #(> % 1) [1 2 3])
         {1 2}
         (filter-level-indices #(= % foo) [foo bar baz]
         {0}"
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
  "Perform a single pass across the vector, using a HashMap of idx:value
  for lookup, a vector of level values, and a byte-seq of indices.

  Ex: => (get-levels-and-indices [1 2 1 1])
      [[1 2] (0 1 0 0)
      => (get-levels-and-indices [foo bar baz foo baz])
      [[foo bar baz] (0 1 2 0 2)]"
  [^clojure.lang.PersistentVector arr-vec]
    (let [^HashMap level-map (HashMap.)
          levels (atom [])
          level-idx (atom 0)
          cnt (count arr-vec)
          type s/int32
          stride (s/byte-size type)
          byte-seq (-> (long cnt) (p/* (long stride)) b/buffer b/byte-seq)]
      ;; Single pass over arr-vec.
      (loop [values arr-vec
             offset 0]
        (when (seq values)
          (let [value (first values)
                map-entry (.get level-map value)]
            (if map-entry
              (s/write-value type byte-seq offset (long map-entry))
              (do
                (swap! levels conj value)
                (.put level-map value @level-idx)
                (s/write-value type byte-seq offset (long @level-idx))
                (swap! level-idx inc))))
          (recur (rest values) (p/+ offset stride))))
      [@levels (s/wrap-byte-seq type byte-seq)]))