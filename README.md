# RESO RCP-019
ANTLR 4 implementation of the RESO RCP-019 grammar, with backwards-compatibility support for RETS 1.9 Validation Expressions.


## 20180506 - using the included `rcp019.g4` file, the following expressions should parse:

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
These functions take 0 or more items. Backwards compatibility with the previous list syntax of `()` has been preserved.

| Expression  | Result |  Comments |
|---|---|---|
|`LIST(1, 2, 2, 3)`|`LIST(1, 2, 2, 3`| Duplicate items are _preserved_.|
|`SET(1, 2, 2, 3)`|`SET(1, 2, 3)`| Duplicate items are _removed_.|

  
* RCP-019.1 also added support for `DIFFERENCE()`, `INTERSECTION()`, and `UNION()`,
which are intended to produce types in the following manner:
  * Operations on _homogeneous_ collections of `LIST()` or `SET()` should 
    return `LIST()` or `SET()`, respectively. For example,
    _`UNION(LIST('a', 'b', 3+5), LIST(6))`_ should return _`LIST('a', 'b', 6, 8)`_.
  * Operations on _heterogenous_ collections of `LIST()` or `SET()`, for 
    instance _`DIFFERENCE(LIST(1, 2, 3), SET(3))`_ should return _`LIST()`_.      

These special functions require at least two arguments of type `LIST()` or `SET()`
   
 
 | Expression  | Result |  Comments |
 |---|---|---|
 |`DIFFERENCE(LIST(1, 2, 3), LIST(1, 2))`|`LIST(3)`|Collection operators require two or more `LIST()` or `SET()` arguments.|
 |`UNION(LIST(1, 2), SET(3))` |`LIST(1, 2, 3)`|Arguments of type `LIST()` are converted to `SET()`.   |
 |`INTERSECTION(SET(DIFFERENCE(LIST(1, 2, 3), SET(1))), SET(2))`|`SET(2)`|Since the return type of `collection` operators is `SET()` or `LIST()`, they can be composed.|
