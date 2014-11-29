(ns caladan.test-agg
  (:require [clojure.test :refer :all]
            [caladan.agg :as agg]
            [caladan.test-helpers :refer :all])
  (:import (java.util HashSet)))

(deftest test-agg
  (testing "Must subset an array of indices given HashSet and Levels"
    (are [in out] (= (agg/subset (get in 0)  (get in 1) (get in 2)) out)
         [(int-array (int-vec)) (HashSet. (int-vec)) []] []
         [(int-array (int-vec 0)) (HashSet. (int-vec 0)) ["foo"]] ["foo"]
         [(int-array (int-vec 0 1)) (HashSet. (int-vec 0 1)) ["foo" "bar"]] ["foo" "bar"]
         [(int-array (int-vec 0 1 0 2)) (HashSet. (int-vec 0 2)) [1 2 3]] [1 1 3]
         [(int-array (int-vec 0 0 1)) (HashSet. (int-vec 0)) [true false]] [true true]
         [(int-array (int-vec 2 1 3 2)) (HashSet. (int-vec 1 2 3)) [1.0 5.0 10.0 20.0]] [10.0 5.0 20.0 10.0]))

  (testing "Must filter level indices and produce a HashSet of level indices"
    (are [in out] (= (agg/filter-level-indices (get in 0) (get in 1)) out)
         [#(= % 1) [1 2 3]] (HashSet. (int-vec 0))
         [#(= (first %) \f) ["foo" "bar" "faz"]] (HashSet. (int-vec 0 2))
         [#(true? %) [true false]] (HashSet. (int-vec 0))))

  (testing "Must get-levels-and-indices"
    (are [in out] (let [[levels indices] (agg/get-levels-and-indices in)]
                    (= levels (get out 0))
                    (= (vec indices) (get out 1)))
         [1 2 3] [[1 2 3][0 1 2]]
         [true false true true] [[true false][0 1 0 0]]
         ["foo" "bar" "baz" "foo" "foo"] [["foo" "bar" "baz"][0 1 2 0 0]]
         [1.0 4.0 1.0 5.0] [[1.0 4.0 5.0][0 1 0 2]]))
  )
