#lang racket

(define (atom? x) (not (or (pair? x) (null? x))))

; Recursive definition of addition
(define plus
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (plus n (sub1 m)))))))

; Recursive definition of subtraction
(define minus
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (minus n (sub1 m)))))))

; Sums a tuple (a list of numbers)
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (plus (car tup) (addtup (cdr tup)))))))

; Recursive definition of multiplication
(define multiply
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (plus n (multiply n (sub1 m)))))))

; Additively combines two tuples of the same length
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons
             (plus (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))

; Recursive definition for `>`
(define greater?
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (greater? (sub1 n) (sub1 m))))))

; Recursive definition for `<`
(define less?
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (less? (sub1 n) (sub1 m))))))

; Equality with `>` and `<`
(define eq
  (lambda (n m)
    (cond
      ((or (greater? n m) (less? n m)) #f)
      (else #t))))

; Exponentiation
(define exponent
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (multiply n (exponent n (sub1 m)))))))

; Whole-integer division (equivalent to Scheme's `quotient`)
(define divide
  (lambda (n m)
    (cond
      ((less? n m) 0)
      (else (add1 (divide (- n m) m))))))

; Length of list of atoms
(define length-lat
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length-lat (cdr lat)))))))

; Return (pick) an atom out of a given (1-indexed) list of atoms, without mutating the list
(define pick
  (lambda (n lat)
    (cond
      ((zero? n) (error "`n` must be greater than zero"))
      ((null? lat) (error "`lat` must not be an empty list"))
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

; Remove and return an atom out of a given list of atoms
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))