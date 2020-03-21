#lang racket        
(define Member?
  (lambda (a L)
    (cond ((null? L) #f)
        ((eq? a (car L)) #t)
        (else (Member? a (cdr L))))))

(define Union
  (lambda (L1 L2)
    (remove-duplicates
    (append L1 L2))))

(define remove-duplicates
  (lambda (L)
    (cond ((null? L) '())
          ((Member? (car L)(cdr L))
          (remove-duplicates (cdr L)))
          (else
           (cons (car L)
                 (remove-duplicates
                  (cdr L)))))))

(define Intersection?
  (lambda (L1 L2)
    (cond ((null? L1) '() )
          ((Member? (car L1) L2)
           (cons (car L1) (Intersection? (cdr L1) L2)))
                 (else (Intersection? (cdr L1) L2))
                 )))

(define (subtract xs ys)
  (if (empty? ys)
      xs
      (subtract (remove (first ys) xs) (rest ys))))

(define Symmetric-Difference
    (lambda (L1 L2)
      (cond ((null? L1) '())
            ((Union (subtract L1 L2)(subtract L2 L1))))))
            

(Member? 'C '(B 2 3))
(Union '(1 2 3) '(3 4 5))
(Intersection? '(1 2 3) '(2 3 4))
(Symmetric-Difference '(1 2 3 4 5 6) '(2 3 4))