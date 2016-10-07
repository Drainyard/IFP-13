;;; week-06_unit-stress-tests.scm
;;; IFP 2016-2017, Q1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 04 Oct 2016

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/IFP16/Lecture-notes/week-06_unit-stress-tests.html

;;;;;;;;;;

(define stress-test-silently
  #t)

(define raises-an-exception?
  (lambda (thunk)
    (guard (the-condition [(error? the-condition)
                           (begin
                             (unless stress-test-silently
                               (begin
                                 (printf "Error in ~s: "
                                         (condition-who the-condition))
                                 (apply printf
                                        (cons (condition-message the-condition)
                                              (condition-irritants the-condition)))
                                 (newline)))
                             #t)]
                          [else
                           (begin
                             (unless stress-test-silently
                               (printf "Another exception is being raised.~n"))
                             #t)])
           (begin
             (thunk)
             #f))))

;;;;;;;;;;

(define stress-test-andmap1
  (lambda (candidate)
    (and (raises-an-exception? (lambda ()
                                 (candidate)))
         (raises-an-exception? (lambda ()
                                 (candidate number?)))
         (raises-an-exception? (lambda ()
                                 (candidate number? '(1 2 3) '(4 5 6))))
         (raises-an-exception? (lambda ()
                                 (candidate number? 'x)))
         (raises-an-exception? (lambda ()
                                 (candidate number? '(1 2 . x))))
         (raises-an-exception? (lambda ()
                                 (candidate "I'm not a procedure" '(1 2))))
         (raises-an-exception? (lambda ()
                                 (candidate number? (lambda () "But I am"))))
         (raises-an-exception? (lambda ()
                                 (candidate number? '(1 2 #t 4 . 5))))
         ;;; etc.
         )))


(unless (stress-test-andmap1 andmap)
  (printf "fail: (stress-test-andmap1 andmap)~n"))

;;;;;;;;;;

(define test-andmap1
  (lambda (candidate)
    (and (equal? (candidate number? '())
                 #t) 
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

;;; We changed our andmap1 from week 4 according to the comments on the last
;;; (accepted) submission.

(define andmap1
  (lambda (p vs)
    (letrec ([visit (lambda (x xs)
                      (cond 
                        [(null? xs)
                         (p x)]
                        [(pair? xs)
                         (and (p x)
                              (visit (car xs) (cdr xs)))]))])
      (if (procedure? p)
          (if (null? vs)
              #t
              (if (list? vs)
                  (visit (car vs) (cdr vs))
                  (errorf 'andmap1
                          "Not a proper list: ~s"
                          vs)))
          (errorf 'andmap1
                  "Not a procedure: ~s"
                  p)))))

;;; This procedure passes both the normal and the stress test.

(unless (stress-test-andmap1 andmap1)
  (printf "fail: (stress-test-andmap1 andmap1)~n"))

(unless (test-andmap1 andmap1)
  (printf "fail: (test-andmap1 andmap1)~n"))

;;; When setting stress-test-silently to false and running the test, one will 
;;; observe the following:

;; > (stress-test-andmap1 andmap1)
;; Another exception is being raised.
;; Another exception is being raised.
;; Another exception is being raised.
;; Error in andmap1: Not a proper list: x
;; Error in andmap1: Not a proper list: x
;; Error in andmap1: Not a procedure: "I'm not a procedure"
;; Error in andmap1: Not a proper list: #<procedure>
;; #t

;;; Which means that the first three test cases raise exceptions due to other
;;; reasons than those we check for explicitly. This is as expected, since
;;; these tests violate the number of arguments to andmap1, which Scheme
;;; automatically checks for us.



;;; Ormap1:

(define stress-test-ormap1
  (lambda (candidate)
    (and (raises-an-exception? (lambda ()
                                 (candidate)))
         (raises-an-exception? (lambda ()
                                 (candidate number?)))
         (raises-an-exception? (lambda ()
                                 (candidate number? '(1 2 3) '(4 5 6))))
         (raises-an-exception? (lambda ()
                                 (candidate number? 'x)))
         (raises-an-exception? (lambda ()
                                 (candidate number? '(1 2 . x))))
         (raises-an-exception? (lambda ()
                                 (candidate "I'm not a procedure" '(1 2))))
         (raises-an-exception? (lambda ()
                                 (candidate number? (lambda () "But I am"))))
         (raises-an-exception? (lambda ()
                                 (candidate number? '(1 2 #t 4 . 5))))
         ;;; etc.
         )))

;;; The test cases are the same as for andmap, since the correct input to ormap1
;;; is the same as to andmap1.
;;; The procedure itself is also written in much the same way as andmap1, except
;;; for the fact that we check whether the input list is proper list in the 
;;; beginning instead of only checking for a pair. This is due to the fact that
;;; the or-call in the pair-case of the recursion would otherwise shortcut 
;;; evaluate to true in many cases where the input is not a proper list but the
;;; procedure applied to one of the first elements is true.
;;; This issue led us to discuss whether andmap1 applied to for example number?
;;; and '(1 2 #t 4 . 5) should return false or raise an error. Since the Scheme
;;; procedure andmap does the latter, we ended up changing the test in andmap1
;;; to list? as well and adding the case to the stress test.

(define ormap1
  (lambda (p vs)
    (letrec ([visit (lambda (x xs)
                      (cond 
                        [(null? xs)
                         (p x)]
                        [(pair? xs)
                         (or (p x)
                             (visit (car xs) (cdr xs)))]))])
      (if (procedure? p)
          (if (null? vs)
              #f
              (if (list? vs)
                  (visit (car vs) (cdr vs))
                  (errorf 'ormap1
                          "Not a proper list: ~s"
                          vs)))
          (errorf 'ormap1
                  "Not a procedure: ~s"
                  p)))))


;;;;;;;;;;
;;; end of week-06_unit-stress-tests.scm

"week-06_unit-stress-tests.scm"
