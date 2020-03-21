#lang racket
;This is a helper function for the member function from lab 3
(define member?
 (lambda ( a L )
   (cond ( (empty? L) #f)
         ( (equal? a (car L)) #t)
               (else (member? a (cdr L))))))

;This function gives us reflexive 
(define Reflexive?
  (lambda (L S)
    (cond ((empty? S) #t);empty set is always reflexive
          ;uses member to compare the two lists then recurse
        ((member? (list (car S) (car S)) L)(Reflexive? L (cdr S)))
        (else #f))))

(Reflexive? '((1 1)(2 2)(3 3)) '(1 2 3))

;This function is a helper function for Symmetric
(define Sym-helper
  ;uses two lists to compare
  (lambda (L L2)
    (cond ((empty? L) #t);empty set it always Symmetric
          ;uses member to check atoms of the list and then recurses
          ((member? (list(cadr(car L))(car(car L))) L2)(Sym-helper(cdr L) L2))
           (else #f))))

;satisfies the requirement for only 1 list 
(define Symmetric?
  (lambda (L)
    (Sym-helper L L)))
     
(Symmetric? '((1 2)(2 3)(2 1)(3 2)))

;This pairs up the sets so that they are in the correct order for manipulation
(define (combine s)
 (cond ((empty? s) empty)
       ((empty? (rest s)) (list s))
       (else
        (let splice ((l '()) (m (first s)) (r (rest s)))
          (append
           (map (lambda (x) (cons m x))
                (combine (append l r)))
           (if (empty? r)
               empty
               (splice (cons m l) (car r) (cdr r))))))))

;This function will return transitive and takes only one argument
(define Transitive?
  (lambda (L)
    (helpT (combine L))))

;This function helps with transitive
(define helpT
  (lambda (L)
    (cond((empty? L) #f)
         ;takes L and checks it then checks it with next helper function
         ((eq? (cadaar L) (caadar L))(helpT2 L))
         ;otherwise keep checking it here
         (else (helpT(cdr L))))))

;function helps check transitive
(define helpT2
  (lambda (L)
    (if (eq? (car(cdr(car(cdr(car L)))))(car(cdr(car(cdr(cdr(car L)))))))
        (helpT3 L)
        (helpT (cdr L)))))

;this function checks transitive and will return true if the two are equal
(define helpT3
  (lambda (L)
    (cond ((eq? (car(caar L)) (caar(cdr(cdr(car L))))) #t)
          ;otherwise send it back to check it again
          (else (helpT(cdr L))))))

(Transitive? '((1 2)(2 3)(1 3)))

;This function is Reflexive-Closure 
(define Reflexive-Closure
  (lambda (L S)
    (cond ((empty? S) L)
          ;this is from our reflexive function
          ((member? (list (car S) (car S)) L)(Reflexive-Closure L (cdr S)))
          ;then if we dont find it to be reflexive we build a new set and add it
          (else (cons (list (car S) (car S)) (Reflexive-Closure L (cdr S)))))))

(Reflexive-Closure '((1 1)(2 2)) '(1 2 3))

;This is Symmetric-Helper, it doesn't work.
;(define Symmetric-Helper
  ;(lambda (L S)
    ;(cond ((empty? S) L)
          ;((member? (list (cadr(car L)) (car(car L))) S)(Symmetric-Helper(cdr L) S))
          ;(else (cons (list(cadr(car L))(car(car L))) (Symmetric-Helper (cdr L) S))))))

;(define Symmetric-Closure
 ; (lambda (L)
  ;  (Symmetric-Helper L L)))

;(Symmetric-Closure '((1 2)(2 3)(2 1)))

;This is a helper function for Transitive
(define Tran-helper
  (lambda (n L)
    (cond ((empty? L) L)
          ;checks if the atoms are equal then constructs a list and recurses
          (else(equal? (cadr n) (caar L)) (cons (list (car n) (cadr (car L))) (Tran-helper n (cdr L)))))))

;This give us Transitive-Closure
(define Transitive-Closure
  (lambda (L)
    (cond ((empty? L) '())
          ;checks if it is a member and removes duplicates before adding them all into a list
          ((member? (car L) L) (remove-duplicates(append L(Tran-helper(car L) L))))
          ;otherwise it recurses
            (else (Transitive-Closure (cdr L))))))

(Transitive-Closure '((1 2)(2 3)))

