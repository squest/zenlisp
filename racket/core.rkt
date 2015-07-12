#lang racket

(define-syntax-rule (def id body)
  (define id body))

(define-syntax-rule (fn binding body)
  (lambda binding body))

(def (square x) (* x x))
(def sqr (fn (x) (* x x)))





