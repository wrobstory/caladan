(ns caladan.agg
  (:require [hiphip.int :as hhi]
            [hiphip.long :as hhl])
  (:import (java.util HashSet HashMap)
           (org.roaringbitmap RoaringBitmap)
           [clojure.lang PersistentVector PersistentArrayMap]))

(set! *warn-on-reflection* true)

;; Categorical Array Handling

(defn get-levels-and-indices
  "Given a vector, return a vector of categorical levels and a primitive int-array
  of indices. Performs membership tests using HashMap.

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
  a HashSet of the indices for these values.

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

(defn group-indices
  "Group a given array of int indices and return a HashMap of {index: Bit-index}

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

(defn cat-subset-on-levels
  "Given a primitive int-array of level indices, a HashSet of level-indices,
  and the level vector, return new levels, indices, and length.
  This subsetter is essentially saying 'Give me only the levels in this given HashSet'

  Ex: => (subset [0 1 2 0 1] {1 2} [foo bar baz])
      [[bar baz] [0 1 0]]
      => (subset [0 1 2 0 1] {0} [foo bar baz])
      [[foo] [0 0]]"
  [^ints indices ^HashSet hashed-indices ^PersistentVector levels]
    (let [return-indices (transient [])
          return-levels (transient [])
          ^HashMap new-level-map (HashMap.)
          level-idx (atom (int 0))]
      (hhi/doarr [x indices]
        (if (.contains hashed-indices x)
          (let [level-value (nth levels x)
                map-entry (.get new-level-map level-value)
                int-level-idx @level-idx]
            (if-not map-entry
              (do
                (.put new-level-map level-value int-level-idx)
                (conj! return-indices int-level-idx)
                (conj! return-levels level-value)
                (swap! level-idx inc))
              (conj! return-indices map-entry)))))
      [(persistent! return-levels) (int-array (persistent! return-indices))]))

(defn cat-subset-on-rows
  "Given an array of level-indices, an Bitmap index of row indices, and levels, use
  the index to generate new indices and levels. This subsetter is essentially
  saying 'Give me only the rows in this bit-index'. The following examples use
  generalized vector/set notation for primitive-arrays and Bitmaps.

  Ex: Given levels [foo, bar, baz];
      Indexes for these levels: [0 1 0 2 1 2], which represents the array [foo bar foo baz bar baz];
      Bitmap index of the level indices: {0 2 5}, which translates to level indices [0 0 2]
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
      [(arr-gen (persistent! values)) val-idx arr-length]))

(defn get-int-arr-comp
  "Get integer array values, value bit-index, and length"
  [input]
    (build-vals-and-index input int-array int))

(defn get-long-arr-comp
  "Get long array values, value bit-index, and length"
  [input]
    (build-vals-and-index input long-array long))

(defn take-num-arr
  "Given a bit-index of value positions, a primitive array of values, a slicing func,
  and n, the number of values you want to take, generate a new set of
  primitive values, value bit-index, and length. Returns all items if there
  are fewer than n.

  Ex: Given values [0 2 4], bit-index indicating vals at {0 1 3}, and n 3 (NA in third position)
      take-num-arr would return [1 2] {0 1} 3 (NA in last position)

  Ex2 => (take-num-arr [0 1 2 5 6] {0 1 2 5 6} 5)
       [[0 1 2 5[ {0 1 2 5} 5]
  Ex3: => (take-num-arr [4 9 10 20] {0 1 4 5} 4)
       [[4 9] {0 1} 4]
  "
  [values ^RoaringBitmap val-idx slicer n]
    (let [new-val-idx (RoaringBitmap.)
          iterator (.getIntIterator val-idx)
          card (.getCardinality val-idx)
          decremented (- n 1)
          f (if (< card decremented) card decremented)]
      (loop [idx 1
             value (.next iterator)]
        (cond
          (= value f) (do
                        (.add new-val-idx value)
                        [(slicer values idx) new-val-idx n])
          (> value f) [(slicer values (- idx 1)) new-val-idx n]
          (< value f) (do
                        (.add new-val-idx value)
                        (recur (inc idx) (.next iterator)))))))

(defmacro filter-num-arr
  "Filter numerical array values for given predicate. Return bitmap of indices and
  filtered values. This will filter out all NA values
  "
  [values val-idx pred do-iter setter slicer arr-type]
    `(let [meta-bitmap# ~(with-meta val-idx {:tag 'RoaringBitmap})
           return-indices# (RoaringBitmap.)
           card# (.getCardinality meta-bitmap#)
           return-array# (make-array ~arr-type card#)
           iterator# (.getIntIterator meta-bitmap#)
           val-count# (atom 0)]
       (~do-iter [x# ~values
                  :let [iter-val# (.next iterator#)]]
         (when (~pred x#)
           (.add return-indices# iter-val#)
           (~setter return-array# @val-count# x#)
           (swap! val-count# inc)))
       [(~slicer return-array# @val-count#) return-indices#]))

(defn filter-int-arr
  "Given an int-array of values, a bit-index indicating value positions, and a predicate,
  filter values and return both the values and the indices of those values. Note:
  this will *always* filter NA/Nil.

  Ex: Given values [0 1 3 4], val-idx {0 1 3 4}, and predicate #(< % 3)
    filter-int-arr would return [0 1] {0 1}
  Ex2: => (filter-int-arr [0 1 2 3 3] {0 1 2 3 4} {2 3})
        [2 3 3] {0 1 2}
  "
  [^ints values ^RoaringBitmap val-idx  pred]
    (filter-num-arr values val-idx pred hhi/doarr hhi/aset int-slicer Integer/TYPE))
