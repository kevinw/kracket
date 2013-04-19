#lang racket

`(
  ((string-length "foo") 3)
  ((vector? (make-vector 5)) #t)
  ((vector-length (make-vector 5)) 5)
  ((vector-ref (make-vector 10) 3) 0))
