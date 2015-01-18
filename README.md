# Caladan


(Proposed) API
--------------
PSA: Literally none of this has been implemented

#### I/O
Read from CSV:
```clojure
(table-from-csv "foo.csv")
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
(sort table {:col1 #(sortfunc)} :type :asc])
```

Filter values where a predicate is matched
```clojure
(filter table {:col1 #(= % "foo") :col2 #{1 2}})
```

Apply a function on columns
```clojure
(apply table {:col1 #(* 4 %) :col2 #(str % "_appender")})
```

Potentially: Apply a function with logic on multiple cols
(apply (fn [:col1 :col2](do-some-stuff-including-filter)))

Reduce columns
```clojure
;; This will probably return a map of {:col reduced_value}?
(reduce table {:col1 + :col2 #(+ %1 %2 2)})
```

Select columns
```clojure
(select table [:col1 :col2])
```

Group a table. This will return a map of group keys to...new tables?
```clojure
(groupby table [:col1])
{:foo-group <Table> :bar-group <Table>}
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

Compose a pipeline of operations:
```clojure
(c<- table
     (filter {:col1 #{1}})
     (groupby [:col2])
     (reduce {:col1 +}))
```

Equivalent to
```sql
select sum(col1)
from table
where col1 = 1
groupby col2;
