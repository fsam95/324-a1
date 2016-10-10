#| Assignment 1 - Racket Query Language  (due Oct 14, 11:50pm)

***Write the names, CDF accounts and student IDs for each of your group members below.***
<Name>, <CDF>, <ID>
<Name>, <CDF>, <ID>
|#
#lang racket
(require "helpers.rkt")

; Function versions for common syntactic forms.
; *Use these in your queries instead of the syntactic forms!!!*
(define (And x y) (and x y))
(define (Or x y) (or x y))
(define (If x y z) (if x y z))

; TODO: After you have defined your macro(s), make sure you add each one
; to the provide statement.
(provide attributes
         tuples
         size
         get-attribute-value
         filter-table
         replace-attr
         extract-values
         SELECT
         cartesian-product
         )

; Part 0: Semantic aliases

#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)

  Returns a list of the attributes in 'table', in the order they appear.
|#
(define (attributes table)
  (list-ref table 0) 
  )

#|
(tuples table)
  table: a valid table

  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.
|#
(define (tuples table)
  (rest table)
  )


#|
(size table)
  table: a valid table

  Returns the number of tuples in 'table'.
|#
(define (size table)
  (length (tuples table))
  )


; Part I "WHERE" helpers; you may or may not wish to implement these.

#|
A function that takes: 
  - a list of attributes
  - a string (representing an attribute)
  - a tuple

  and returns the value of the tuple corresponding to that attribute.
|#
(define (get-attribute-value attribute-list attribute tuple)
  (list-ref tuple (index-of attribute-list attribute))
  )

#|
A function that takes:
  - f: a unary function that takes a tuple and returns a boolean value
  - table: a valid table

  and returns a new table containing only the tuples in 'table'
  that satisfy 'f'.
|#
(define (filter-table f table)
  (filter f (tuples table))
  )

#|
A function 'replace-attr' that takes:
  - x 
  - a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corrresponding value 
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.
|#
(define (replace-attr x attributes)
    (lambda (tuple) 
      (if (member x attributes)
        (get-attribute-value attributes x tuple)
        x
      )
    )
  )


(define (extract-values query tuple attribute-list) ;s attribute list is query
  (map (lambda (attribute) 
         ((replace-attr attribute attribute-list) tuple) 
         )
         query)

  )

(define (selection query filtered-table)
  (if 
    (equal? query WILDCARD)
    filtered-table
    (append (list query) (map (lambda (tuple)
           (extract-values query tuple (list-ref filtered-table 0)))
         (tuples filtered-table)))
    )
  )


(define WILDCARD *)

(define-syntax SELECT
  (syntax-rules (FROM WHERE )
    [(SELECT query FROM table) 
     (selection query table)]
;    [(SELECT query FROM [<table-one>] <next-table> ...)
;     (#| TODO: fill in body|#)]
    [(SELECT query FROM table WHERE filter-function)
     (filter-table filter-function (selection query table))]
    ))
#|
(cartesian-product 
|#
(define (cartesian-product table-one table-two)
  (append (list (append (attributes table-one) (attributes table-two))) (append* (map (lambda (x) (map (lambda (y) (append x y)) (tuples table-two))) (tuples table-one))))
  )


