(ns caladan.test-helpers
  (:require [vertigo.core :as v]
            [vertigo.structs :as s]))

(defn make-vert
  ([sequence]
    (make-vert s/int32 sequence))
  ([type sequence]
    (v/marshal-seq type sequence)))

(defn make-vec
  [^:s/int32 vert-arr]
    (v/doreduce [x vert-arr] [return []]
      (conj return x)))