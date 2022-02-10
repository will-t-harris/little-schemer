#lang scheme
(define (atom? x) (not (or (pair? x) (null? x))))


; Removes an atom from any(?) list of S-expressions
(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

; occur* test
(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup)) "should equal" '((coffee) ((tea)) (and (hick))) (newline)


; Inserts the atom `new` to the right of `old` anywhere in a list of S-expressions
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

; insertR* test
(insertR* 'roast 'chuck '((how much (wood) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood)))
"should equal"
'((how much (wood) could ((a (wood) chuck roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck roast wood)) (newline)


; Returns the number of times an atom exists in a list of S-expressions
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l)) (occur* a (cdr l)))))))
         
; occur* test
(occur* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy))) "should equal" 5 (newline)


; Replaces all instances of `old` with `new` in a list of S-expressions
(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons
           new
           (subst* new old (cdr l))))
         (else
          (cons
           (car l)
           (subst* new old (cdr l))))))
      (else
       (cons
        (subst* new old (car l))
        (subst* new old (cdr l)))))))

; subst* test
(subst* 'orange 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))
"should equal"
'((orange) (split ((((orange ice))) (cream (orange)) sherbet)) (orange) (bread) (orange brandy)) (newline)


; Inserts atom `new` to the left of `old` anywhere in a list of S-expressions
(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new
                (cons old
                      (insertL* new old (cdr l)))))
         (else
          (cons (car l)
                (insertL* new old (cdr l))))))
      (else
       (cons (insertL* new old (car l))
             (insertL* new old (cdr l)))))))

; insertL* test
(insertL* 'pecker 'chuck '((how much (wood) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood)))
"should equal"
'((how much (wood) could ((a (wood) pecker chuck)) (((pecker chuck))) (if (a) ((wood pecker chuck))) could pecker chuck wood)) (newline)


; Returns true if the atom exists in the list of S-expressions
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a) (member* a (cdr l))))
      (else
       (or (member* a (car l)) (member* a (cdr l)))))))
         
; member* tests
(member* 'chips '((potato) (chips ((with) fish) (chips)))) "should equal" #t (newline)
(member* 'boop '((potato) (chips ((with) fish) (chips)))) "should equal" #f (newline)


; Finds the leftmost atom in a non-empty list of S-expressions that doesn't contain the empty list
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

; leftmost tests
(leftmost '((potato) (chips ((with) fish) (chips)))) "should equal" 'potato (newline)
(leftmost '(((hot) (tuna (and))) cheese)) "should equal" 'hot (newline)


; Determines if two lists are equal
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
      (else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))
      
; eqlist? tests
(eqlist? '(strawberry ice cream) '(strawberry cream ice)) "should equal" #f (newline)
(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))) "should equal" #t (newline)
(eqlist? '() '()) "should equal" #t (newline)


; Determines equality between S-expressions
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eq? s1 s2))
      ((or (atom? s1) (atom? s2) #f))
      (else (eqlist? s1 s2)))))


; eqlist? rewritten using equal? (eqlist2?)
(define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))))))

; tests
(eqlist2? '(strawberry ice cream) '(strawberry cream ice)) "should equal" #f (newline)
(eqlist2? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))) "should equal" #t (newline)
(eqlist2? '() '()) "should equal" #t (newline)


; re-worked rember; removes a S-expression from a list of S-expressions
(define rember
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (rember s (cdr l)))))))

; rember tests
(rember 'boop '(goop toop boop shoop)) "should equal" '(goop toop shoop) (newline)