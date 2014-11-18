(ns caladan.test-arrays
  (:require [clojure.test :refer :all]
            [caladan.arrays :as ca]
            [clojure.test.check :as tc])
  (:import (java.util HashSet)))

(deftest test-arrays
  (testing "Must subset an array of indices given HashSet and Levels"
    (are [in out] (= (ca/subset (get in 0)  (get in 1) (get in 2)) out)
         [(long-array []) (HashSet. []) []] []
         [(long-array [0]) (HashSet. [0]) ["foo"]] ["foo"]
         [(long-array [0 1]) (HashSet. [0 1]) ["foo" "bar"]] ["foo" "bar"]
         [(long-array [0 1 0 2]) (HashSet. [0 2]) [1 2 3]] [1 1 3]
         [(long-array [0 0 1]) (HashSet. [0]) [true false]] [true true]
         [(long-array [2 1 3 2]) (HashSet. [1 2 3]) [1.0 5.0 10.0 20.0]] [10.0 5.0 20.0 10.0]))

  (testing "Must filter level indices and produce a HashSet of level indices"
    (are [in out] (= (ca/filter-level-indices (get in 0) (get in 1)) out)
         [#(= % 1) [1 2 3]] (HashSet. [0])
         [#(= (first %) \f) ["foo" "bar" "faz"]] (HashSet. [0 2])
         [#(true? %) [true false]] (HashSet. [0])))

  (testing "Must get-levels-and-indices"
    (are [in out] (let [[levels indices] (ca/get-levels-and-indices in)]
                    (= levels (get out 0))
                    (= (vec indices) (get out 1)))
         [1 2 3] [[1 2 3][0 1 2]]
         [true false true true] [[true false][0 1 0 0]]
         ["foo" "bar" "baz" "foo" "foo"] [["foo" "bar" "baz"][0 1 2 0 0]]
         [1.0 4.0 1.0 5.0] [[1.0 4.0 5.0][0 1 0 2]]))

  (testing "Creates indexed array"
    (are [in out] (let [idx-array (ca/make-indexed-array in)
                        levels (:levels idx-array)
                        indices (vec (:indices idx-array))]
                    (= levels (get out 0))
                    (= indices (get out 1)))
         [1 2 3] [[1 2 3][0 1 2]]))

  (testing "Takes values from indexed array"
    (are [in out] (let [idx-array (ca/make-indexed-array in)
                        taken (ca/take idx-array (get out 0))]
                    (= taken (get out 1)))
         [1 2 3] [1 [1]]
         [1.0 5.0 3.0 4.0] [3 [1.0 5.0 3.0]]))

  (testing "filters indexed array"
    (are [in out] (let [idx-array (ca/make-indexed-array in)
                        filtered (ca/filter idx-array (get out 0))]
                    (= filtered (get out 1)))
         [1 2 3 1 1 2] [#(<= % 2) [1 2 1 1 2]]
         [true false true true] [#(true? %) [true true true]]
         ["foo" "fa" "barr" "boooo"] [#(> (count %) 4) ["boooo"]])))
