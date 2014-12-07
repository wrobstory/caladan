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

  (testing "Gets vector from categorical array"
    (are [in out] (let [cat-array (ca/make-categorical-array in)
                        taken (ca/get-vector cat-array (get out 0))]
                    (= taken (get out 1)))
         [1 2 3] [1 [1]]
         [1.0 5.0 3.0 4.0] [3 [1.0 5.0 3.0]]))

  (testing "Filters indexed array"
    (are [in out] (let [cat-array (ca/make-categorical-array in)
                        select-arr (ca/select cat-array (get out 0))]
                    (every? true? [(= (.levels select-arr) (get out 1))
                                   (= (vec (.indices select-arr)) (get out 2))
                                   (= (ca/vec select-arr) (get out 3))]))
         [1 2 3 1 1 2] [#(<= % 2) [1 2] [0 1 0 0 1] [1 2 1 1 2]]
         [true false true true] [#(true? %) [true] [0 0 0] [true true true]]
         ["foo" "fa" "barr" "boooo"] [#(> (count %) 4) ["boooo"] [0] ["boooo"]]))
  )
