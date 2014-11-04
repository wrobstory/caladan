(ns caladan.parsers
  "Parsing Utilities for data i/o"
  (:import (org.apache.commons.lang3.math NumberUtils)))

(defn iterate-parsers
  "Given a string to-parse, a Seq of parsers, and a conditional for which
  to the parser returns 'False', parse the value"
  [^String to-parse, false-conditional, parsers]
    (loop [parsers parsers]
      (let [parsed ((first parsers) to-parse)]
        (if (not= false-conditional parsed)
          parsed
          (recur (rest parsers))))))

(defn parse-number
  "Parse a String to either a Long or a Float. Will try to parse Long before
  parsing Float"
  [^String to-parse]
    (iterate-parsers to-parse 0[#(NumberUtils/toLong %) #(NumberUtils/toFloat %)]))

(defn parse
  "Parse a given value to a Long, Float, Boolean, or return as String"
  [^String to-parse]
    (cond
      (re-matches #"\d*\.?\d+" to-parse) (parse-number ^String to-parse)
      (re-matches #"(?i) true|True" ^String to-parse) true
      (re-matches #"(?i) false|False" ^String to-parse) false
      :else to-parse))