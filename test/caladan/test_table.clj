(ns caladan.test-table
  (:require [clojure.test :refer :all]
            [caladan.table :as ct]
            [caladan.arrays :as ca]))

(deftest test-table
  (testing "Table throws when arrays of unequal size passed"
    (is (thrown-with-msg? java.lang.IllegalArgumentException #"Arrays must be of same length!"
         (ct/make-table {:col1 (ca/make-integer-array [1 2]) :col2 (ca/make-integer-array [1])}))))
    )
