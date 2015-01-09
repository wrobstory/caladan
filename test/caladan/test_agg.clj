(ns caladan.test-agg
  (:require [clojure.test :refer :all]
            [caladan.agg :as agg]
            [caladan.test-helpers :refer :all])
  (:import (java.util HashSet)
           (org.roaringbitmap RoaringBitmap)))

(deftest test-agg
  (testing "Must catch nil"
    (is (true? (agg/nil-catcher nil?)))
    (is (false? (agg/nil-catcher #(< % 3))))
    (is (true? (agg/nil-catcher #(or (nil? %) (== % 1))))))

  (testing "Must get-levels-and-indices"
    (are [in out] (let [coll []
                        [levels indices] (agg/get-levels-and-indices in)]
                    (every? true? [(= levels (get out 0))
                                   (= (vec indices) (get out 1))]))
      [1 2 3] [[1 2 3][0 1 2]]
      [true false true true] [[true false][0 1 0 0]]
      ["foo" "bar" "baz" "foo" "foo"] [["foo" "bar" "baz"][0 1 2 0 0]]
      [1.0 4.0 1.0 5.0] [[1.0 4.0 5.0][0 1 0 2]]))

  (testing "Must filter level indices and produce a HashSet of level indices"
    (are [in out] (= (agg/filter-level-indices (get in 0) (get in 1)) out)
      [#(= % 1) [1 2 3]] (HashSet. (int-vec 0))
      [#(= (first %) \f) ["foo" "bar" "faz"]] (HashSet. (int-vec 0 2))
      [#(true? %) [true false]] (HashSet. (int-vec 0))))

  (testing "Must perform subset given hash set of levels"
    (are [in out] (let [[levels indices] (agg/cat-subset-on-levels (get in 0)  (get in 1) (get in 2))]
                    (every? true? [(= levels (get out 0))
                                   (= (vec indices) (get out 1))]))
      [(int-array []) (HashSet. (int-vec)) []] [[] []]
      [(int-array [0]) (HashSet. (int-vec 0)) ["foo"]] [["foo"] [0]]
      [(int-array [0 1]) (HashSet. (int-vec 0 1)) ["foo" "bar"]] [["foo" "bar"] [0 1]]
      [(int-array [0 1 0 2]) (HashSet. (int-vec 0 2)) [1 2 3]] [[1 3] [0 0 1]]
      [(int-array [0 0 1]) (HashSet. (int-vec 0)) [true false]] [[true] [0 0]]
      [(int-array [2 1 3 2]) (HashSet. (int-vec 1 2 3)) [1.0 5.0 10.0 20.0]] [[10.0 5.0 20.0] [0 1 2 0]]
      [(int-array [0 2 1 0]) (HashSet. (int-vec 1 2)) ["a" "b" "c"]] [["c" "b"] [0 1]]))

  (testing "Must group indices"
    (are [in out] (= (agg/group-indices in) out)
      (int-array [0]) {(int 0) (bitmapper [0])}
      (int-array [0 1 0]) {(int 0) (bitmapper [0 2])
                           (int 1) (bitmapper [1])}
      (int-array [0 1 2 1 0]) {(int 0) (bitmapper [0 4])
                               (int 1) (bitmapper [1 3])
                               (int 2) (bitmapper [2])}))

  (testing "Must subset and reindex levels and indices"
    (are [in out] (let [[levels indices] (agg/cat-subset-on-rows (get in 0)  (get in 1) (get in 2))]
                    (every? true? [(= levels (get out 0))
                                   (= (vec indices) (get out 1))]))
      [(int-array [1 2 0 1 3]) (bitmapper [0 1 4]) ["a" "b" "c" "d"]] [["b" "c" "d"] [0 1 2]]
      [(int-array [0 1 0 0 2]) (bitmapper [0 2 3]) ["a" "b" "c"]] [["a"] [0 0 0]]
      [(int-array [1 1 0 2 1]) (bitmapper [0 2]) ["a" "b" "c"]] [["b" "a"] [0 1]]))

  (testing "Must create int values and value index"
    (are [in out] (let [[values val-idx length] (agg/get-int-arr-comp in)]
                    (every? true? [(= (vec values)  (get out 0)),
                                   (= val-idx (get out 1))
                                   (= length (get out 2))]))
      [1 2 3] [(vectof :int 1 2 3) (bitmapper [0 1 2]) 3]
      [1 2 nil 3] [(vectof :int 1 2 3) (bitmapper [0 1 3]) 4]
      [1 nil 2 nil nil 4] [(vectof :int 1 2 4) (bitmapper [0 2 5]) 6]))

  (testing "Must create long values and value index"
    (are [in out] (let [[values val-idx length] (agg/get-long-arr-comp in)]
                    (every? true? [(= (vec values) (get out 0)),
                                   (= val-idx (get out 1))
                                   (= length (get out 2))]))
      [nil nil 1] [(vectof :long 1) (bitmapper [2]) 3]
      [1 2 nil 3] [(vectof :long 1 2 3) (bitmapper [0 1 3]) 4]))

  (testing "Must slice arrays"
    (are [in out] (= (vec ((:slicer in) (:arr in) (:length in))) out)
      {:slicer agg/int-slicer :arr (int-array [1 2 3 4]) :length 2} (vectof :int 1 2)
      {:slicer agg/int-slicer :arr (int-array [1 2 3 4]) :length 0} []
      {:slicer agg/int-slicer :arr (int-array [1 2 3 4]) :length 10} (vectof :int 1 2 3 4)
      {:slicer agg/long-slicer :arr (long-array [1 2 3 4]) :length 3} [1 2 3]))

  (testing "Must take numeric arrays"
    (are [in out] (let [[values val-idx length] (agg/take-num-arr (:vals in) (:val-idx in) (:slicer in) (:n in))]
                    (every? true? [(= (vec values) (:vals out)),
                                   (= val-idx (:val-idx out))
                                   (= length (:l out))]))

      {:vals (int-array [1 2 4]) :val-idx (bitmapper [0 1 3]) :slicer agg/int-slicer :n 3}
      {:vals [1 2] :val-idx (bitmapper [0 1]) :l 3}

      {:vals (long-array [0 1 2 5 6]) :val-idx (bitmapper [0 1 2 5 6]) :slicer agg/long-slicer :n 5}
      {:vals [0 1 2] :val-idx (bitmapper [0 1 2]) :l 5}

      {:vals (long-array [4 9 10 20 40]) :val-idx (bitmapper [0 2 4 6 8]) :slicer agg/long-slicer :n 6}
      {:vals [4 9 10] :val-idx (bitmapper [0 2 4]) :l 6}
      ))

  (testing "Must filter int arrays"
    (are [in out] (let [[values val-idx-orig val-idx-new] (agg/filter-int-arr (:vals in) (:val-idx in) (:l in)(:pred in))]
                    (every? true? [(= (vec values) (:vals out)),
                                   (= val-idx-orig (:val-idx-orig out))
                                   (= val-idx-new (:val-idx-new out))]))

      {:vals (int-array [1 2 4]) :val-idx (bitmapper [0 1 3]) :l 4 :pred #(< % 3)}
      {:vals [1 2] :val-idx-orig (bitmapper [0 1]) :val-idx-new (bitmapper [0 1])}

      {:vals (int-array [0 1 1 2 2]) :val-idx (bitmapper [0 1 5 6 7]) :l 8 :pred #{1 2}}
      {:vals [1 1 2 2] :val-idx-orig (bitmapper [1 5 6 7]) :val-idx-new (bitmapper [0 1 2 3])}

      {:vals (int-array [3 8 9 2 1]) :val-idx (bitmapper [0 2 4 5 7]) :l 8 :pred #(>= % 8)}
      {:vals [8 9] :val-idx-orig (bitmapper [2 4]) :val-idx-new (bitmapper [0 1])}

      {:vals (int-array [0 1 2]) :val-idx (bitmapper [0 2 4]) :l 5 :pred nil?}
      {:vals [] :val-idx-orig (bitmapper [1 3]) :val-idx-new (bitmapper [])}

      {:vals (int-array [5 5 5]) :val-idx (bitmapper [5 6 7]) :l 8 :pred #(or (nil? %) (== % 5))}
      {:vals [5 5 5] :val-idx-orig (bitmapper [0 1 2 3 4 5 6 7]) :val-idx-new (bitmapper [0 1 2])}

      {:vals (int-array [1]) :val-idx (bitmapper [1]) :l 3 :pred #(or (nil? %) (== % 1))}
      {:vals [1] :val-idx-orig (bitmapper [0 1 2]) :val-idx-new (bitmapper [0])}
      ))

  (testing "Must filter-reduce int arrays"
    (are [in out] (let [red (agg/filter-reduce-int-arr (:pred in) (:reducer in) (:init in) (:values in))]
                    (= red out))
      {:pred pos? :reducer + :init 0 :values (int-array [-1 -2 0 1 2 3])} 6

      {:pred #{1} :reducer - :init 10 :values (int-array [1 1 1 1])} 6

      {:pred #(> % 3) :reducer + :init 0 :values (int-array [0 1 2 3 4 5])} 9

      {:pred #{2} :reducer #(* %1 %2) :init 1 :values (int-array [1 1 1 2 2 2])} 8

      {:pred #(= (/ % 2) 4) :reducer #(+ %1 %2 10) :init 0 :values (int-array [1 2 8 3 4 8])} 36))
)