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

(unless (test-andmap1 andmap1)
  (printf "fail: (test-andmap1)~n"))

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
;;; arguments yields false

(define ormap1
  (lambda (p vs)
    (letrec ([visit (lambda (ws)
                      (if (null? ws)
                          #f
                          (if (null? (cdr ws))
                              (p (car ws))
                              (or (p (car ws))
                                  (visit (cdr ws))))))])
      (visit vs))))

(unless (test-ormap1 ormap1)
  (printf "fail: (test-ormap1)~n"))

;;;;;;;;;;

;;; end of week-04_tail-calls.scm

"week-04_tail-calls.scm"
