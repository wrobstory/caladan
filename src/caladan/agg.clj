(ns caladan.agg
  (:require [hiphip.int :as hhi]
            [vertigo.structs :as s]
            [vertigo.bytes :as b]
            [vertigo.primitives :as p]
            [vertigo.core :as v])
  (:import (java.util HashSet HashMap)
           (org.roaringbitmap RoaringBitmap)))

(set! *warn-on-reflection* true)

(defn group-indices
  "Group a given array of long indices and return a HashMap of {index: Bitmap}

  Ex: => (group-indices [0 1 0 1 2 1])
     {0 #<RoaringBitmap {0,2}>, 1 #<RoaringBitmap {1,3,5}>, 2 #<RoaringBitmap {4}>}"
  [^ints indices]
    (let [^HashMap return-map (HashMap.)]
      (hhi/doarr [[i x] indices]
        (let [^RoaringBitmap map-entry (.get return-map x)]
          (if map-entry
            (.add map-entry i)
            (let [^RoaringBitmap new-bitmap (RoaringBitmap.)]
              (do
                (.add new-bitmap i)
                (.put return-map x new-bitmap))))))
      return-map))

(defn subset-and-reindex
  "Given an array of level-indices, an Bitmap index of row indices, and levels, use
  the index to generate new indices and levels. The following examples use
  generalized vector/set notation for byte-sequences and Bitmaps.

  Ex: Given levels [foo, bar, baz];
      Indexes for these levels: [0 1 0 2 1 2], which represents the array [foo bar foo baz bar baz];
      Bitmap index of the level indices: {0 2 5}, which represent levels indices [0 0 2]
      Reindex with new indices and levels given the bitmap index:
        levels: [foo baz]
        level-indices: [0 0 1]

  Ex2: => (categorical-subset-and-reindex [1 2 0 1 3] {0 1 4} [a b c d])
       [[b c d] [0 1 2]]
  Ex3: => (categorical-subset-and-reindex [0 1 0 0 2] {0 2 3} [a b c])
       [[a] [0 0 0]]
  Ex4: => (categorical-subset-and-reindex [1 1 0 2 1] {0 2} [a b c])
       [[b a] [0 1]]"
  [^ints level-indices ^RoaringBitmap row-indices ^clojure.lang.PersistentVector levels]
    (let [return-indices (make-array Integer/TYPE (.getCardinality row-indices))
          return-levels (atom [])
          ^HashMap new-level-map (HashMap.)
          level-idx (atom 0)
          iterator (.getIntIterator row-indices)]
      (hhi/doarr [[i x] return-indices]
        (let [row-idx (.next iterator)
              row-value (aget ^ints level-indices row-idx)
              level-value (get levels row-value)
              map-entry (.get new-level-map level-value)
              int-level-idx (int @level-idx)]
          (if-not map-entry
            (do
              (.put new-level-map level-value int-level-idx)
              (aset ^ints return-indices i int-level-idx)
              (swap! return-levels conj level-value)
              (swap! level-idx inc))
            (aset ^ints return-indices i (int map-entry)))))
      [@return-levels return-indices]))

(defn subset
  "Given a int-array of level indices, a HashSet of hashed-indices, and the level vector
  , return a vector of subsetted values.

  Ex: => (subset [0 1 2 0 1] #{1 2} [foo bar baz])
      [bar baz bar]
      => (subset [0 1 2 0 1] #{0} [foo bar baz])
      [foo foo]"
  [^ints indices ^HashSet hashed-indices ^clojure.lang.PersistentVector levels]
    (let [return-vector (transient [])]
      (hhi/doarr [x indices]
        (if (.contains hashed-indices x)
          (conj! return-vector (nth levels x))))
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
                              (.add hashed-levels (int (get item 0)))
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
    (let [level-map (HashMap.)
          levels (atom [])
          level-idx (atom 0)
          indices (make-array Integer/TYPE (count arr-vec))]
      ;; Single pass over arr-vec.
      (loop [values arr-vec
             idx 0]
        (when (seq values)
          (let [value (first values)
                map-entry (.get level-map value)]
            (if map-entry
              (aset ^ints indices idx (int map-entry))
              (do
                (swap! levels conj value)
                (.put level-map value @level-idx)
                (aset ^ints indices idx (int @level-idx))
                (swap! level-idx inc))))
          (recur (rest values) (inc idx))))
      [@levels indices]))