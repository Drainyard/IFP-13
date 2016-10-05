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
         ;;; etc.
         )))

(unless (stress-test-andmap1 andmap)
  (printf "fail: (stress-test-andmap1 andmap)~n"))

;;;;;;;;;;

;;; end of week-06_unit-stress-tests.scm

"week-06_unit-stress-tests.scm"
