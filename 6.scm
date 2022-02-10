#lang scheme

(define (atom? x) (not (or (pair? x) (null? x))))

; Determines whether a representation of an arithmetic expression contains only numbers besides '+ 'x and '^
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

; numbered? tests
(numbered? '(3 + (4 x 5))) "should equal" #t (newline)
(numbered? '(1 + 2 - 3 ^ 5)) "should equal" #t (newline)
(numbered? '(boop + (3 - 2))) "should equal" #f (newline)


; Returns the natural value of a numbered arithmetic expression
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+)
       (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) 'x)
       (* (value (car nexp)) (value (car (cdr (cdr nexp))))))
      (else
       (expt (value (car nexp)) (value (car (cdr (cdr nexp)))))))))

; value tests
(value '(1 + (2 x 3))) "should equal" 7 (newline)
(value '(4 + (2 ^ 4))) "should equal" 20 (newline)