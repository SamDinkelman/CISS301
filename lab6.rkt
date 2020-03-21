#lang racket
;this function compares the first atom to these operators
(define compare
  (lambda (a L2)
    (cond ((equal? a 'if)L2) 
          ((equal? a 'and)L2)
          ((equal? a 'or)L2)
          ((equal? a 'iff)L2)
          ((equal? a 'not)L2)
          (else (list a)))))

;this is a helper for collect-prop-variable it does the bulk of the work
(define helper
  (lambda (L L2)
    (cond ((empty?  L)L)
          ((list? (car L)) (remove-duplicates (append (helper (car L) L2) (helper (cdr L) L2))))
          (else (append (compare (car L) L2) (helper (cdr L) L2))))))

;makes it so that we only take in one list
(define collect-prop-variable
  (lambda (L)
    (helper L '())))

(collect-prop-variable '(A and (not B)))

;this defines the substitute function
(define substitute
  (lambda (L A A2)
    (cond ((empty? L)L)
          ((list? (car L))(append(list(substitute(car L) A A2))(substitute(cdr L) A A2)))
          ((equal? (car L) A)(cons A2(substitute(cdr L) A A2)))
          (else (cons(car L)(substitute(cdr L) A A2))))))
(substitute '(C or (D or D)) 'D #f)

;compares elements to find if they are operators or outcomes
(define Evaluate-WFF
 (lambda (L)
   (cond ((not(list? L)) L)
         ((equal? (length L)3) (cond ((equal? (cadr L) 'or) (or (Evaluate-WFF (car L)) (Evaluate-WFF (cddr L))))
                                     ((equal? (cadr L) 'and) (and (Evaluate-WFF (car L)) (Evaluate-WFF (cddr L))))
                                     ((equal? (cadr L) 'implies) (implies (Evaluate-WFF (car L)) (Evaluate-WFF (cddr L))))
                                     ((equal? (cadr L) 'iff) (and (implies (Evaluate-WFF (car L)) (Evaluate-WFF (cddr L))) (implies (Evaluate-WFF (cddr L)) (Evaluate-WFF (car L)))))))
         ((equal? (length L)2) (not (Evaluate-WFF (cdr L))))
         (else (Evaluate-WFF(car L))))))

(Evaluate-WFF '(#t or (not #t)))
          
