;;; week-05_self-interpreter-continued-and-ended.scm
;;; IFP 2016-2017, Q1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 26 Sep 2016

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/IFP16/Lecture-notes/week-05_self-interpreter-continued-and-ended.html

;;;;;;;;;;

;;; Solution of Exercise 1:

(define make-tower
  (lambda (height)
    (letrec ([visit (lambda (n)
                      (if (= n 0)
                          '((lambda (x y z) (+ z z)) (time 1) (time 10) (time 100))
                          (list representation-of-interpret (list 'quote (visit (1- n))))))])
      (if (and (integer? height)
               (not (negative? height)))
          (visit height)
          (errorf 'make-tower
                  "not a non-negative integer: ~s"
                  height)))))

;;;;;;;;;;

;;; end of week-05_self-interpreter-continued-and-ended.scm

"week-05_self-interpreter-continued-and-ended.scm"
