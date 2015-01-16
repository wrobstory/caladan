# Caladan


(Proposed) API
--------------
PSA: Literally none of this has been implemented

#### I/O
Read from CSV:
```clojure
table-from-csv "foo.csv")
```
Read from SQL:
```clojure
(table-from-sql "select foo, bar from baztable;" sqlconn)
```

#### Data Manipulation

Take n values from table
```clojure
(take table 10)
```

Slice table
```clojure
(slice table 5 10)
```

Get distinct values for given columns (or the whole table)
```clojure
;; Returns a new table (two cols wide) with distinct values from col1 and col2
(distinct table [:col1 :col2])

;; Returns distinct (unique) rows for the entire table
(distinct table)
```

Sort values for given vector of cols
```clojure
(sort table [:col1])
```

Filter values where a predicate is matched
```clojure
(filter table {:col1 #(= % "foo") :col2 #{1 2}})
```

Apply a function on columns
```clojure
(apply table {:col1 #(* 4 %) :col2 #(str % "_appender")})
```

Reduce columns
```clojure
;; This will probably return a map of {:col reduced_value}?
(reduce table {:col1 + :col2 #(+ %1 %2 2)})
```

Select columns
```clojure
(select [:col1 :col2])
```

Select columns with where-filtering predicates on other cols
```clojure
(select [:col1] :where {:col2 #{1 2}})
```

Select columns with where-filtering predicates and a reducer
```clojure
;; "Select col1 where col2 is in the set #{1 2} and reduce the resulting values"
(select [:col1] :where {:col2 #{1 2}} :reduce +)
```

Group a table. This will return a map of group keys to...new tables?
```clojure
(groupby table [:col1])
{:foo-group <Table> :bar-group <Table>}
```

Group a table, apply a function to given cols in a given group
```clojure
(groupby table [:col1] :apply {:foo-group  {:col2 #(+ % 1)}})
{:foo-group <TableWithApply> :bar-group <Table>}
```

Group a table, apply a function to all groups, then reduce (full split-apply-combine)
```clojure
;; This groups by col1, applies the function to col2 in all groups, then reduces all cols with +
(groupby table [:col1] :apply {:col2 #(+ % 1)} :reduce +)
```

Group a table with a where predicate
```
(groupby table [:col1] :where {:col2 #{1 2}} :reduce *)
```

There are shortcuts for common aggregations
```clojure
(groupby table [:col1] :reduce :mean)
(groupby table [:col1] :reduce :sum)
(groupby table [:col1] :reduce max)
```

Join two tables
```clojure
(left-join table1 table2 :on [:col1])
(right-join table1 table2 :on [:col1])
(inner-join table1 table2 :on [:col1 :col2])
(outer-join table1 table2 :on [:col2])
```

Set operations
```clojure
(intersect table1 table2)
(union table1 table2)
(diff table1 table2)
```