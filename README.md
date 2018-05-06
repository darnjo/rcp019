# rets19
ANTLR 4 implementation of the RETS 1.9 grammar



## 20180506 - using the included rets19.g4 file, the following expressions should parse:

* Basic arithmetic  
`3 + 5`

* Bracketed RETSNAMEs  
`[ListPrice] > 0`

* RCP61 added support for unbracketed RETSNAMEs  
`ListPrice > 0`

* To get current value of any field, just use the `DICTNAME` of that field  
`ListPrice`

* RETS had support for the LAST keyword, which now works on all fields  
to get the old value  
`LAST Status != "Sold"`

* Which is useful for field change detection  
`ListPrice != LAST ListPrice`

* Function calls are also supported, as long as they don't use  
any of the RETSNAME keywords  
`foo()`  
`foo ('bar', 'baz', 42)`  

* Lists of expressions are also supported  
`(ListPrice, Status, 3+5, LAST Status, (1, 2, 3))`

* Supports complex expressions with grouping  
`ListPrice > 5.01 .AND. (1, 2, 3) .CONTAINS. 3
    .OR. (Status .IN. ('Active', 'Pending') .AND. .USERLEVEL. != "Admin")`

* Which parses differently than  
`ListPrice > 5.01 .AND. (1, 2, 3) .CONTAINS. 3
    .OR. Status .IN. ('Active', 'Pending') .AND. .USERLEVEL. != "Admin"`

