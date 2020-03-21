#lang racket
;This program does some mathematical computations

(define Sum1
  (lambda (n)
    ;checks if n is greater than or equal to 1
    (if (>= n 1)
        ;do some computations if it is 
          (+ (+ (Sum1 (- n 1)) (+ (* n 3) 1)))
          ;else return 0
          0)))
(Sum1 5)

(define Sum2
  (lambda (n)
    ;checks if n is not zero 
    (if (zero? n)
        ;if it is zero then return 0 
        0
        ;else do the computations
        (+ (Sum2 (- n 1)) (* (- (* (- n 1)(- n 1))(* n n))(- (* (- n 1)(- n 1))(* n n)))))))
(Sum2 3)

;this function will sum up all numbers up to x
(define Sum3-helper
  (lambda (x)
    ;checks if n is not zero 
    (if (zero? x)
        ;if it is zero then return 0 
        0
        (+ (Sum3-helper (- x 1)) x))))

(define Sum3
  (lambda (n)
    ;checks if n is greater than zero 
    (if (zero? n)
        ;if it is zero then return 0
        0
        ;we use a recursive call to the helper and a recursive call to this function to get all the sums summed up
        (+ (+ (Sum3-helper (- n 1)) n) (Sum3 (- n 1))))))

(Sum3 5)

(define Sum4
  (lambda (n)
    ;checks if n is not zero 
    (if (zero? n)
        ;if it is zero then return 0 
        0
        ;do the same as number 3 since the middle sigma doesn't have any affect on the computations
        (+ (+ (Sum3-helper (- n 1)) n) (Sum3 (- n 1))))))

(Sum4 5)