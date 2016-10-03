#lang plai
; (abridged-test-output #t) TODO: check the effect of this

(require "database.rkt")
(require "helpers.rkt")

(define sample-tuple-one '("David" 20 #t))
(define sample-tuple-two '("Jen" 30 #t))
(define sample-tuple-three '("Paul" 50 #f))
(define sample-attributes-one '("Name" "Age" "LikesChocolate"))
(define sample-table-one (list sample-tuple-one sample-tuple-two sample-tuple-three))

(define (f tuple) (> (list-ref tuple 1) 25))


(test (index-of sample-attributes-one "LikesChocolate") 
      2)

(test (index-of sample-attributes-one "NonExistentAttribute")
      -1)

(test (get-attribute-value sample-attributes-one "Name" sample-tuple-one)
      "David")

(test (filter-table f sample-table-one)
      (list sample-tuple-two sample-tuple-three))

(test ((replace-attr "Age" sample-attributes-one) sample-tuple-one)
      20)

(test ((replace-attr "Date of Birth" sample-attributes-one) sample-tuple-one)
      "Date of Birth")

