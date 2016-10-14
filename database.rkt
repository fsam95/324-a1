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
         string-index-of
         modify-query
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
(define (filter-table f table) ;(equal? "Name" "David")
  (cons (list-ref table 0) (filter f (tuples table)))
  )

#|
A function 'replace-attr' that takes:
  - attribute-name 
  - a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'attribute-name' is in the list of attributes, return the corrresponding value 
      in the tuple.
    - Otherwise, just ignore the tuple and return 'attribute-name'.
|#
(define (replace-attr attribute-name  attributes)
    (lambda (tuple) 
      (if (member attribute-name attributes)
        (get-attribute-value attributes attribute-name tuple)
        attribute-name
      )
    )
  )

#|
(define (unary pred attributes attribute-index)
  (lambda (tuple)
    ((list-ref pred 0) (get-attribute-value attributes attribute-name tuple))
    )
  )
|#
  

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

#|
Probably have to 
create the filter-function on your own
|#
(define-syntax SELECT
  (syntax-rules (FROM WHERE )
    [(SELECT <query> FROM <table>) 
     (selection <query> <table>)]
    [(SELECT <query> FROM <table> WHERE <pred>)
     (selection <query> (filter-table (replace <pred> <table>) <table>))]
;     (filter-table (replace (list <pred>) (selection <query> <table>)))] 
   [(SELECT <query> FROM [<table> <name>])
    (SELECT <query> FROM <table>)]
   [(SELECT <query> FROM [<table1> <name>] <next-pair> ...)
    (cartesian-product (selection (modify-query <query> <name>) <table1>) 
                    (SELECT <query> FROM <next-pair> ...))]
    ))


#|
;Remove irrelevant items in this query 
TODO
|#
(define (modify-query query name [lst '()])
  (if (equal? query WILDCARD) query
    (if (null? query) lst
      (if (string-contains? (car query) ".") 
        (if (equal? (substring (car query) 0 (string-index-of (car query) ".")) name)
          (modify-query (rest query) name (append lst (list (substring (car query) (+ (string-index-of (car query) ".") 1)))))
          (modify-query (rest query) name lst)
          )
        (modify-query (rest query) name (append lst (list (car query))))
        )
      )
    )
  )

#|
(define (combine-tables table1 table2 [i 0] [combined '()])
  (if (null? table1) combined
    (combine-tables table1 table2 (+ 1 i) (append combined (append (list-ref table1 i) (list-ref table2 i))))
    )
  )
|#

#|
(cartesian-product 
|#
(define (cartesian-product table-one table-two)
  (append (list (append (attributes table-one) (attributes table-two))) (append* (map (lambda (x) (map (lambda (y) (append x y)) (tuples table-two))) (tuples table-one))))
  )

#|
Return index of first occurence of character char
|#
(define (string-index-of str char [pos 0])
  (if (equal? str "")
    -1 
    (if (equal? (substring str 0 1) char) pos
      (string-index-of (substring str 1) char (+ 1 pos)))
    )
  )

(define-syntax replace 
  (syntax-rules ()

    ;Recursive step when given compound expression
    [(replace (<pred> ...) table) ;(< "Age" 20)
     (unary (list pred) (list-ref table 0) )
     ]
    ; The base case
    [(replace <pred> table) ;LikesChocolate
     (replace-attr <pred> (list-ref table 0))
     ]
  ))
    

