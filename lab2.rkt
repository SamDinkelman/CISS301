#lang racket
(define LHS
  (lambda (n)
    ;evaluates to true as long as n is greater than 0
    (if (> n 0)
        (+ (LHS (- n 1)) (* n n))
        ;returns 0 if n reaches 0
        0)))

(define RHS
  (lambda (n)
    (/ (* (* (+ n 1) n) (+ (* n 2) 1)) 6)))

(define NLHS
  (lambda(n x)
        (+ (NLHS (- n 1)) n)
    n
    ;evaluates to true as long as n is greater than 0
    (if (> n 0)
        (* x x)
        ;returns 0 if n reaches 0
         0)))

(define NRHS
  (lambda (n)
    ;evaluates to true as long as n is greater than 0
    (if (> n 0)
        (+ (NRHS (- n 1)) (* (* n n) n))
        0)))