(ns caladan.test-agg
  (:require [clojure.test :refer :all]
            [caladan.agg :as agg]
            [caladan.test-helpers :refer :all])
  (:import (java.util HashSet)
           (org.roaringbitmap RoaringBitmap)))

(deftest test-agg
  (testing "Must get-levels-and-indices"
    (are [in out] (let [[levels indices] (agg/get-levels-and-indices in)]
                    (= levels (get out 0))
                    (= (vec indices) (get out 1)))
      [1 2 3] [[1 2 3][0 1 2]]
      [true false true true] [[true false][0 1 0 0]]
      ["foo" "bar" "baz" "foo" "foo"] [["foo" "bar" "baz"][0 1 2 0 0]]
      [1.0 4.0 1.0 5.0] [[1.0 4.0 5.0][0 1 0 2]]))

  (testing "Must filter level indices and produce a HashSet of level indices"
    (are [in out] (= (agg/filter-level-indices (get in 0) (get in 1)) out)
      [#(= % 1) [1 2 3]] (HashSet. (int-vec 0))
      [#(= (first %) \f) ["foo" "bar" "faz"]] (HashSet. (int-vec 0 2))
      [#(true? %) [true false]] (HashSet. (int-vec 0))))

  (testing "Must subset an array of indices given HashSet and Levels"
    (are [in out] (= (agg/subset (get in 0)  (get in 1) (get in 2)) out)
      [(int-array []) (HashSet. (int-vec)) []] []
      [(int-array [0]) (HashSet. (int-vec 0)) ["foo"]] ["foo"]
      [(int-array [0 1]) (HashSet. (int-vec 0 1)) ["foo" "bar"]] ["foo" "bar"]
      [(int-array [0 1 0 2]) (HashSet. (int-vec 0 2)) [1 2 3]] [1 1 3]
      [(int-array [0 0 1]) (HashSet. (int-vec 0)) [true false]] [true true]
      [(int-array [2 1 3 2]) (HashSet. (int-vec 1 2 3)) [1.0 5.0 10.0 20.0]] [10.0 5.0 20.0 10.0]))

  (testing "Must group indices"
    (are [in out] (= (agg/group-indices in) out)
      (int-array [0]) {(int 0) (bitmapper [0])}
      (int-array [0 1 0]) {(int 0) (bitmapper [0 2])
                           (int 1) (bitmapper [1])}
      (int-array [0 1 2 1 0]) {(int 0) (bitmapper [0 4])
                               (int 1) (bitmapper [1 3])
                               (int 2) (bitmapper [2])}))

  (testing "Must subset and reindex levels and indices"
    (are [in out] (let [[levels indices] (agg/cat-subset-and-reindex (get in 0)  (get in 1) (get in 2))]
                    (= levels (get out 0))
                    (= (vec indices) (get out 1)))
      [(int-array [1 2 0 1 3]) (bitmapper [0 1 4]) ["a" "b" "c" "d"]] [["b" "c" "d"] [0 1 2]]
      [(int-array [0 1 0 0 2]) (bitmapper [0 2 3]) ["a" "b" "c"]] [["a"] [0 0 0]]
      [(int-array [1 1 0 2 1]) (bitmapper [0 2]) ["a" "b" "c"]] [["b" "a"] [0 1]]))
  )
