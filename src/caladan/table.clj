(ns caladan.table
  (:require [caladan.arrays :as ca])
  (:refer-clojure :exclude [take]))

;; Helper funcs

(defn map-vals
"Given a map and a function, returns the map resulting from applying the function to each value."
  [m f mapper]
    (zipmap (keys m) (mapper f (vals m))))

;; Tables

(defprotocol TableProt
  (take [this n])
  (to-map [this]))

(deftype Table [columns]

  TableProt

  (take [this n]
    (let [result (map-vals (.columns this) #(ca/take % n) pmap)]
      (Table. result)))

  (to-map [this]
    (let [col-map (.columns this)]
      (zipmap (keys col-map) (map #(ca/get-vector %1 (.length %1)) (vals col-map)))))

  )

(defmethod clojure.core/print-method caladan.table.Table
  [^caladan.table.Table tbl ^java.io.Writer w]
    (doall (map #(.write w (str % "\n")) (.columns tbl))))

(defn- length-comp-throw
  "Throw error if array lengths not equal"
  [current_length ^caladan.arrays.Array array]
    (if (not= current_length (.length array))
      (throw (java.lang.IllegalArgumentException. "Arrays must be of same length!"))
      current_length))

(defn make-table
  "Make Caladan table out of map of Caladan Arrays. Arrays *must* be the same
  length

  Ex: (make-table {:col1 intarr1 :col2 intarr2})
  "
  [arrays]
    (reduce length-comp-throw (.length (second (first arrays))) (vals arrays))
    (Table. arrays))