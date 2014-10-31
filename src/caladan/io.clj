(ns caladan.io
  "Utilities for data i/o into Clojure Maps/Vectors as well as
   Caladan Arrays"
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn get-csv-seq
  "Get lazy seq of vectors via data.csv"
  [filepath]
  (with-open [in-file (io/reader filepath)]
    (doall
      (csv/read-csv in-file))))

(defn fill-map-vector
  "Given a lazy seq of vectors and header row number, return Map of Vectors in
  the form {:col1 [1 2 3...] col2 [4 5 6...]"
  [csv-seq header]
  (let [header-row (map keyword (nth csv-seq 0))
        col-map (apply array-map (interleave header-row (repeatedly (count header-row) #(transient []))))]
    (doseq [[idx row] (map-indexed vector csv-seq)
            :let [zipped-row (map vector (keys col-map) row)]
            :when (> idx (+ header 1))]
      (reduce #(update-in %1 [(first %2)] conj! (second %2)) col-map zipped-row))
    (reduce #(assoc %1 (first %2) (vec (persistent! (second %2)))) {} col-map)))

(defn create-caladan-dataset
  "Create caladan dataset from results of create-map-vector"
  [map-vector]
    map-vector)

(defn read-csv
  "Reads a CSV into either a Map of Vectors, or a Caladan Table

  Options:
    :to-map (default false)
    :delim (default \\,)
    :quote (default \\\")
    :header (default 0) Default header row. Intervening rows will be skipped."
  [filepath & {:keys [to-map delim quote header]
               :or {to-map false delim \, quote \" header 0}}]
  (let [csv-seq (get-csv-seq filepath)
        map-vector (fill-map-vector csv-seq header)]
    (if to-map map-vector (create-caladan-dataset map-vector))))
