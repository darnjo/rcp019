# RCP-019
ANTLR 4 implementation of the RETS 1.9 and RESO RCP-019 grammar


## 20180506 - using the included `rets19.g4` file, the following expressions should parse:

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
    
    
## 20190304 - added `collection` support    
    
* RCP-019.1 added support for `collections` which include `LIST()` and `SET()`. 
Backwards compatibility with the previous list syntax of `()` has been preserved.
  * `LIST(1, 2, 2, 3)`&nbsp;&nbsp;&nbsp;`//new LIST() with elements 1, 2, 2, 3`
  * `SET(1, 2, 2, 3)`&nbsp;&nbsp;&nbsp;`//interpreter should produce SET(1, 2, 3)`
  
* RCP-019.1 also added support for `DIFFERENCE()`, `INTERSECTION()`, and `UNION()`,
which produce items of type `SET()`. 
These items require at least two arguments of type `LIST()` or `SET()`
  * `DIFFERENCE(LIST(), LIST())`&nbsp;&nbsp;`//should produce LIST()`
  * `UNION(LIST(1, 2), SET(3))`&nbsp;&nbsp;&nbsp;`//should produce SET(1, 2, 3)`
  * `INTERSECTION(SET(DIFFERENCE(LIST(1, 2, 3), LIST(2, 3))), LIST('a', 'b'))`&nbsp;&nbsp;&nbsp;`//should produce SET()`