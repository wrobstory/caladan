(require '[caladan.arrays :as ca]
         '[clojure.test.check.generators :as gen]
         '[criterium.core :as crit])

;; Cardinality ~10-10000, random sample of 100000 integers
(def card10 (gen/sample (gen/choose 0 10) 1000000))
(def card100 (gen/sample (gen/choose 0 100) 1000000))
(def card1000 (gen/sample (gen/choose 0 1000) 1000000))
(def card100000 (gen/sample (gen/choose 0 100000) 1000000))

(println "Benchmarking make-indexed-array...")
(crit/with-progress-reporting (crit/quick-bench (ca/make-indexed-array card10)))

(def myarr100000 (ca/make-indexed-array card100000))
(println "Benchmarking filter array...")
(crit/with-progress-reporting (crit/quick-bench (ca/filter myarr100000 #(< % 5))))

(println "Benchmarking standard filter...")
(crit/with-progress-reporting (crit/quick-bench (vector (filter #(< % 5) card10))))