(ns caladan.test-helpers)

(defmacro int-vec
  [& nums]
    `(vector-of :int ~@nums))
