;;; week-04_tail-calls.scm
;;; IFP 2016-2017, Q1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 20 Sep 2016

;;;;;;;;;;

(define test-andmap1
  (lambda (candidate)
    (and (equal? (candidate number? '())
                 #t)  ;;; <------******------ why not #f?
         (equal? (candidate number? '(1 2 3))
                 #t)
         (equal? (candidate number? '(1 "2" 3))
                 #f)
         (equal? (candidate (lambda (x) x) '(1 2 3))
                 3)
         (equal? (candidate (lambda (x) x) '(#f 3))
                 #f)
         (equal? (candidate (lambda (x) x) '(#f 3 4 5))
                 #f)
         ;;;
         )))

;;; The first test should evaluate to true because applying and to zero
;;; arguments yields true

(define andmap1-not-properly-tail-recursive
  (lambda (p vs)
    (letrec ([visit (lambda (ws)
                      (if (null? ws)
                          #t
                          (and (p (car ws))
                               (visit (cdr ws)))))])
      (visit vs))))

(define andmap1
  (lambda (p vs)
    (letrec ([visit (lambda (ws)
                      (if (null? ws)
                          #t
                          (if (null? (cdr ws))
                              (p (car ws))
                              (and (p (car ws))
                                   (visit (cdr ws))))))])
      (visit vs))))

;;; The (p vn) call is in tail-position, because it is:
;;; the consequent of an if-expression, which is
;;; the alternative of an if-expression, which is
;;; in the body of a lambda-abstraction, which is
;;; in the body of a letrec-expression, which finally is
;;; in the body of a lambda-abstraction.


(define andmap1-alt
  (lambda (p vs)
    (letrec ([visit (lambda (ws a)
                      (if (or (null? ws)
                              (equal? #f a))
                          a
                          (visit (cdr ws) (and a (p (car ws))))))])
      (visit vs #t))))

;;; In this case we keep track of the latest result og applying p and if this 
;;; was false or the lits of arguments is empty, we return it. The 
;;; (p (car ws)) calls are not in tail-position though.

(define andmap1-alt-prettier?
  (lambda (p vs)
    (letrec ([visit (lambda (ws a)
                      (cond
                        [(or (null? ws)
                              (equal? #f a))
                         a]
                        [(pair? ws)
                          (visit (cdr ws) (and a (p (car ws))))]
                        [else
                         (errorf 'andmap1-alt-prettier?
                                 "Not a proper input list: ~s"
                                 vs)]))])
      (visit vs #t))))

;;; It seems to us that the structural recursion is more obvious in this version


(unless (test-andmap1 andmap1)
  (printf "fail: (test-andmap1 andmap1)~n"))

;;; testing andmap1-alt

(unless (test-andmap1 andmap1-alt)
  (printf "fail: (test-andmap1 andmap1-alt)~n"))

(unless (test-andmap1 andmap1-alt-prettier?)
  (printf "fail: (test-andmap1 andmap1-alt-prettier?)~n"))

;;;;;;;;;;

(define test-ormap1
  (lambda (candidate)
    (and (equal? (candidate number? '())
                 #f)  ;;; <------******------ why not #t?
         (equal? (candidate number? '(1 2 3))
                 #t)
         (equal? (candidate number? '("1" "2" 3))
                 #t)
         (equal? (candidate number? '("1" "2" "3"))
                 #f)
         (equal? (candidate (lambda (x) x) '(1 2 3))
                 1)
         (equal? (candidate (lambda (x) x) '(#f #f 3 4))
                 3)
         (equal? (candidate (lambda (x) x) '(#f #f))
                 #f)
         ;;;
         )))

;;; The first test should evaluate to false because applying or to zero
;;; arguments yields false.

(define ormap1
  (lambda (p vs)
    (letrec ([visit (lambda (ws)
                      (cond 
                        [(null? ws)
                         #f]
                        [(pair? ws)
                         (if (null? (cdr ws))
                              (p (car ws))
                              (or (p (car ws))
                                  (visit (cdr ws))))]
                        [else
                         (errorf 'ormap1
                                 "Not a proper input list: ~s"
                                 vs)]))])
      (visit vs))))


;;; The (p vn) call is in tail-position, since it is:
;;; the consequent of an if-expression, which is
;;; the consequent of a cond-expression, which is
;;; in the body of a lambda-abstraction, which is
;;; in the body of a letrec-expression, which finally is
;;; in the body of a lambda-abstraction.


;;; Testing ormap

(unless (test-ormap1 ormap1)
  (printf "fail: (test-ormap1 ormap1) ~n"))


;;;;;;;;;;

;;; end of week-04_tail-calls.scm

"week-04_tail-calls.scm"
