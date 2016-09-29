#lang racket

(provide index-of)

(define (index-of lst item [pos 0])
  (if (equal? pos (length lst)) -1
    (if (equal? (list-ref lst pos) item) pos
      (index-of lst item (+ pos 1))
      )
    )
  )
