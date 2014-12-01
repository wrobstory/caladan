(ns caladan.agg
  (:require [hiphip.int :as hhi]
            [hiphip.long :as hhl]
            (caladan.arrays.CategoricalArray))
  (:import (java.util HashSet HashMap)
           (org.roaringbitmap RoaringBitmap)
           [clojure.lang PersistentVector PersistentArrayMap]))

(set! *warn-on-reflection* true)

;; Categorical Array Handling

(defn get-levels-and-indices
  "Perform a single pass across the vector, using a HashMap of idx:value
  for lookup, a vector of level values, and a byte-seq of indices.

  Ex: => (get-levels-and-indices [1 2 1 1])
      [[1 2] (0 1 0 0)
      => (get-levels-and-indices [foo bar baz foo baz])
      [[foo bar baz] (0 1 2 0 2)]"
  [^PersistentVector arr-vec]
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

(defn filter-level-indices
  "Given a predicate and a set of levels, filter the level values and return
  a HashSet of the level indices

  Ex: => (filter-level-indices (> % 1) [1 2 3])
         {1 2}
         (filter-level-indices (= % foo) [foo bar baz]
         {0}"
  [pred ^PersistentVector levels]
    (let [hashed-levels (HashSet.)
          level-filter (fn [hl item]
                          (if (pred (get item 1))
                            (do
                              (.add hashed-levels (int (get item 0)))
                              hashed-levels)
                            hashed-levels))]
      (reduce level-filter hashed-levels (map-indexed vector levels))
      hashed-levels))

(defn subset
  "Given a int-array of level indices, a HashSet of hashed-indices, and the level vector
  , return a vector of subsetted values.

  Ex: => (subset [0 1 2 0 1] {1 2} [foo bar baz])
      [bar baz bar]
      => (subset [0 1 2 0 1] {0} [foo bar baz])
      [foo foo]"
  [^ints indices ^HashSet hashed-indices ^PersistentVector levels]
    (let [return-vector (transient [])]
      (hhi/doarr [x indices]
        (if (.contains hashed-indices x)
          (conj! return-vector (nth levels x))))
      (persistent! return-vector)))

(defn group-indices
  "Group a given array of long indices and return a HashMap of {index: Bitmap}

  Ex: => (group-indices [0 1 0 1 2 1])
     {0 <RoaringBitmap {0,2}>, 1 <RoaringBitmap {1,3,5}>, 2 <RoaringBitmap {4}>}"
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

(defn cat-subset-and-reindex
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
  [^ints level-indices ^RoaringBitmap row-indices ^PersistentVector levels]
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

;; Numeric array Handling

;; Helper funcs
(defmacro slicer
  "Create an array slicer for a given type"
  [arr slice arr-type iterator setter]
    `(let [arr-len# (count ~arr)
           n# (if (> ~slice arr-len#) arr-len# ~slice)
           new-arr# (make-array ~arr-type n#)]
       (~iterator [[i# x#] ~arr :range [0 n#]]
         (~setter new-arr# i# x#))
       new-arr#))

(defn int-slicer
  "Slice primitive integer arrays for given length"
  [arr length]
    (slicer arr length Integer/TYPE hhi/doarr hhi/aset))

(defn long-slicer
  "Slice primitive long arrays for given length"
  [arr length]
    (slicer arr length Long/TYPE hhl/doarr hhl/aset))

(defn build-vals-and-index
  "Given a vector input, an array generator (Ex: int-array), and a scalar-type,
  Return a bitmap indicating value positions, a primitive array of values,
  and total array length"
  [input arr-gen scalar-type]
    (let [val-idx (RoaringBitmap.)
          arr-length (count input)
          values (transient [])]
      (loop [in input
             idx 0]
        (when (seq in)
          (let [value (first in)
                is-nan (nil? value)]
            (when-not is-nan
              (.add val-idx idx)
              (conj! values (scalar-type value))))
          (recur (rest in) (inc idx))))
      [val-idx (arr-gen (persistent! values)) arr-length]))

; (defn subset-vals-and-NA
;   "Given existing NA index and primitive array, subset based on given length"
;   [^RoaringBitmap val-idx values length]
;     (let [new-val-idx (RoaringBitmap.)
;           iterator (.getIntIterator val-idx)
;           n length]
;       (loop [idx 0
;              value (.next iterator)]
;         (cond
;           (= value n) (do
;                         (.add new-val-idx value)
;                         [new-val-idx (int-slicer values idx) n])
;           (> value n) [new-val-idx (int-slicer values idx) n]
;           (< value n) (do
;                         (.add new-val-idx value)
;                         (recur (inc idx) (.next iterator)))))))

(defn get-int-arr-comp
  "Get components for int array"
  [input]
    (build-vals-and-index input int-array int))

(defn get-long-arr-comp
  "Get components for long array"
  [input]
    (build-vals-and-index input long-array long))