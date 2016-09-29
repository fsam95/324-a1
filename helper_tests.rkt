#lang plai
; (abridged-test-output #t) TODO: check the effect of this

(require "database.rkt")
(require "helpers.rkt")

(define sample-tuple '("David" 20 #t))
(define sample-attributes '("Name" "Age" "LikesChocolate"))


(test (index-of sample-attributes "LikesChocolate") 
      2)

(test (index-of sample-attributes "NonExistentAttribute")
      -1)
