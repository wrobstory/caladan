(ns caladan.test-arrays
  (:require [clojure.test :refer :all]
            [caladan.arrays :as ca])
  (:import (java.util HashSet)))

(deftest test-arrays
  (testing "Creates categorical array"
    (are [in out] (let [cat-array (ca/make-categorical-array in)
                        levels (.levels cat-array)
                        indices (vec (.indices cat-array))]
                    (every? true? [(= levels (get out 0))
                                   (= indices (get out 1))]))
         [1 2 3] [[1 2 3][0 1 2]]))

  (testing "Slices values from categorical array"
    (are [in out] (let [cat-array (ca/make-categorical-array in)
                        taken (ca/slice cat-array (get out 0))]
                    (= taken (get out 1)))
         [1 2 3] [1 [1]]
         [1.0 5.0 3.0 4.0] [3 [1.0 5.0 3.0]]))

  (testing "filters indexed array"
    (are [in out] (let [cat-array (ca/make-categorical-array in)
                        filtered (ca/select cat-array (get out 0))]
                    (= filtered (get out 1)))
         [1 2 3 1 1 2] [#(<= % 2) [1 2 1 1 2]]
         [true false true true] [#(true? %) [true true true]]
         ["foo" "fa" "barr" "boooo"] [#(> (count %) 4) ["boooo"]]))
  )
