#| Assignment 1 - Racket Query Language  (due Oct 14, 11:50pm)

***Write the names, CDF accounts and student IDs for each of your group members below.***
S M Farhan Samir, samirs, 1000660439   
Eddie Quan, quaneddi, 999903476
|#
#lang racket
(require "helpers.rkt")

; Function versions for common syntactic forms.
; *Use these in your queries instead of the syntactic forms!!!*
(define (And x y) (and x y))
(define (Or x y) (or x y))
(define (If x y z) (if x y z))
(define ns (make-base-namespace))
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
         name-cols
         cartesian-product-one
         cartesian-product-two
         strip-prefix
         strip-names
         rename-unique-attrs
         replace
         substitute
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

#|
(index-of lst item [pos])
 lst: list of items
 item: item to search for
 pos: index in list currently being checked

 Returns the index of an item in a list
|#
(define (index-of lst item [pos 0])
  (if (equal? pos (length lst)) -1
    (if (equal? (list-ref lst pos) item) pos
      (index-of lst item (+ pos 1))
      )
    )
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
  (cons (attributes table) (filter f (tuples table)))
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

(define (rename-unique-attrs table)
  (let* ([raw-attribute-list (strip-names (attributes table))]
    [fixed-attributes (map (lambda (attr)
           (if (equal? 1 (count (lambda (other-attr) (equal? other-attr (strip-prefix attr))) raw-attribute-list))
             (strip-prefix attr)
             attr)
           )
         (attributes table))])
    (append (list fixed-attributes) (tuples table))
    )
  )

(define (strip-names attributes)
  (map (lambda (attribute) 
         (strip-prefix attribute)) attributes)
  )

(define (strip-prefix attribute)
  (if (string-contains? attribute ".") 
    (substring attribute (+ (string-index-of attribute ".") 1))
    attribute))

#|
# Rename all attributes in table 2 by prepending the prefix 'name-two' 
# onto every attribute
|#
(define (cartesian-product-one table-one table-two name-two)
  (let ([combined-attributes (append (attributes table-one) (name-cols (attributes table-two) name-two))])
    (append (list combined-attributes) (cartesian-product (tuples table-one) (tuples table-two))))
  )

(define (name-cols attrs name)
  (map (lambda (attr)
      (string-append (string-append name ".") attr))
       attrs)
  )

(define (cartesian-product-two table-one table-two name-one name-two)
  (let ([combined-attributes (append (name-cols (attributes table-one) name-one) (name-cols (attributes table-two) name-two))]) 
    (begin 
      (append (list combined-attributes) (cartesian-product (tuples table-one) (tuples table-two))))
      )
  )

(define (cartesian-product table1 table2)
    (append* (map (lambda (x) (map (lambda (y) (append x y)) table2 )) table1))
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

#|
  (substitute lambdas tuple)
  lambdas: a list of lambdas (results of replace-attr on <pred> elements)
  tuple: tuple that is used as argument for lambdas

  Substitutes generalized expression (e.g '(+ "Age" "NumYearsTaught"))
  with values from tuple (e.g '(+ 30 4))
|#
(define (substitute lambdas tuple)
    (if (list? lambdas) 
      (if (null? lambdas) '()
        (if (list? (first lambdas)) ;nested list of lambdas (represents nested <pred>) 
          (cons (substitute (first lambdas) tuple) (substitute (rest lambdas) tuple)) 
          (cons ((first lambdas) tuple) (substitute (rest lambdas) tuple))))
        (list (lambdas tuple)))
  )

#|
  (order-by lambdas table)
  lambdas: a list of lambdas (results of replace-attr on <pred> elements)
  table: a table

  Helper function that sorts tuples according to 
|#
(define (order-by lambdas table)
  (append (list (attributes table)) (sort (tuples table) > #:key (lambda (tuple) 
           (let ([subbed-expr (substitute lambdas tuple)])
              (if (number? (first subbed-expr)) ;in case a number is returned (instead of function call); happens when ORDER by <pred> is a single attribute
                (first subbed-expr)
                (eval subbed-expr ns))
             )))
          )
  )

#|
  (where lambdas table)
  lambdas: a list of lambdas (results of replace-attr on <condition> elements)
  table: a table (duh)
  
  Helper function that filters tuples according to
  <pred> specified in WHERE clause
|#
(define (where lambdas table)
  (filter-table (lambda (tuple)
    (let ([subbed-expr (substitute lambdas tuple)])
      (if (equal? (length subbed-expr) 1) (first subbed-expr) ;in case (#t/#f) is returned (in place of function call); happens when WHERE <condition> is a single attribute
        (eval (substitute lambdas tuple) ns)) )) table)
  )

#|
  Converts attributes and operators into lambdas
  that return value of a tuple corresponding to that
  tuple--or the operator itself
|#
(define-syntax replace
  (syntax-rules ()
    [(replace (exprs ...) attrs) ; (> "Age" 25)
     (let ([func (list (replace exprs attrs) ...)])
       func)
     ]
    [(replace atom attributes)
     (replace-attr atom attributes)]
    ))

(define-syntax SELECT
  (syntax-rules (FROM WHERE ORDER BY)
    ;simple select
    [(SELECT <query> FROM <table>) 
     (selection <query> (rename-unique-attrs <table>))]

    ;simple select, matches literal table
    [(SELECT <query> FROM [<quote> <table>]) 
     (selection <query> (rename-unique-attrs (<quote> <table>)))]

    ;select from multiple tables 
    [(SELECT <query> FROM [<table1> <name1>] [<table2> <name2>] <next-table> ...)
     (SELECT <query> FROM (cartesian-product-two <table1> <table2> <name1> <name2>) <next-table> ...)]
    [(SELECT <query> FROM <table1> [<table2> <name2>] <next-table> ...)
     (SELECT <query> FROM (cartesian-product-one <table1> <table2> <name2>) <next-table> ...)]
    
    ;select with filtering
    [(SELECT <query> FROM <table> WHERE <pred>)
     (SELECT <query> FROM (where (replace <pred> (attributes <table>)) 
                                        (SELECT * FROM <table>)))]
    ;select with ordering
    [(SELECT <query> FROM <table> ORDER BY <pred>)
     (SELECT <query> FROM (order-by (replace <pred> (attributes (SELECT * FROM <table>)))
                                    (SELECT * FROM <table>)))]

    ;select, multiple tables, order by, where
    [(SELECT <query> FROM <table> WHERE <condition> ORDER BY <pred>)
     (let ([filtered-table (SELECT * FROM (where (replace <condition> (attributes <table>)) 
                                        (SELECT * FROM <table>)))]) 
       (SELECT <query> FROM (order-by (replace <pred> (attributes filtered-table))
                                      (SELECT * FROM filtered-table))))]
    ))
