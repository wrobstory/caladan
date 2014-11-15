(ns caladan.test-arrays
  (:require [clojure.test :refer :all]
            [caladan.arrays :as ca]
            [clojure.test.check :as tc])
  (:import (java.util HashSet)))

(deftest test-arrays
  (testing "Must subset an array of indices given HashSet and Levels"
    (are [in out] (= (ca/subset (get in 0)  (get in 1) (get in 2)) out)
         [(long-array []) (HashSet. []) {}] []
         [(long-array [0]) (HashSet. [0]) {0 "foo"}] ["foo"]
         [(long-array [0 1]) (HashSet. [0 1]) {0 "foo" 1 "bar"}] ["foo" "bar"]
         [(long-array [0 1 0 2]) (HashSet. [0 2]) {0 1 1 2 2 3}] [1 1 3]
         [(long-array [0 0 1]) (HashSet. [0]) {0 true 1 false}] [true true]
         [(long-array [2 1 3 2]) (HashSet. [1 2 3]) {0 1.0 1 5.0 2 10.0 3 20.0}] [10.0 5.0 20.0 10.0])))
