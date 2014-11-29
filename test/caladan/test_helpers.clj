(ns caladan.test-helpers
  (:import (org.roaringbitmap RoaringBitmap)))

(defmacro int-vec
  "Create a vector of integers"
  [& nums]
    `(vector-of :int ~@nums))

(defn bitmapper
  "Create a bitmap from a given seq"
  [sequence]
    (let [bitmap (RoaringBitmap.)]
      (doseq [x sequence]
        (.add bitmap x))
      bitmap))
