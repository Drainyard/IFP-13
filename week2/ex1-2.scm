;;; week-02_unit-test-paraphernalia.scm
;;; IFP 2016-2017, Q1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 05 Sep 2016

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/IFP16/Lecture-notes/week-02_unit-test-paraphernalia.html

;;;;;;;;;;

(define try-candidate
  (lambda (name candidate expected-output . input)
    (or (equal? expected-output
                (apply candidate input))
        (begin
          (printf "~s: error for ~s~n" name input)
          #f))))

;;;;;;;;;;

(define test-and-all
  (lambda (name candidate)
    (and (try-candidate name
                        candidate
                        #t
                        #t #t #t)
         (try-candidate name
                        candidate
                        #f
                        #t #f #t)
         (try-candidate name
                        candidate
                        #t
                        #t "hej" #t)
         (try-candidate name
                        candidate
                        #t
                        #t 2 #t)
         ;;; etc.
         )))

(define and-all
  (lambda bs_init
    (letrec ([visit (lambda (bs)
                      (or (null? bs)
                          (and (car bs)
                               (visit (cdr bs)))))])
      (visit bs_init))))

(unless (test-and-all 'and-all and-all)
  (printf "fail: (test-and-all 'and-all and-all)~n"))

;;;;;;;;;;

;;; Exercise 1
;;; ----------
;;; 
;;; Define your very own version of ``andmap``.
;;; 
;;; * Reminder: ``andmap`` was described `in the PL lecture notes
;;;   <http://users-cs.au.dk/danvy/dProgSprog16/Lecture-notes/week-5_map.html#mapping-procedures-over-proper-lists-continued>`_.
;;; 
;;; * Hint: start by defining a unit-test procedure.

(define test-andmap
  (lambda (name candidate)
    (and (try-candidate name
                        candidate
                        #t
                        number?
                        '(0 1/2 3.14))
         (try-candidate name
                        candidate
                        #f
                        number?
                        '(0 "1/2" 3.14))
         (try-candidate name
                        candidate
                        #t
                        <
                        '(0 1 4 3)
                        '(1 2 5 4))
         (try-candidate name
                        candidate
                        7
                        +
                        '(0 1 4 3)
                        '(1 2 5 4))
         (try-candidate name
                        candidate
                        #t
                        <
                        '())
 
         ;;; etc.
         )))

(unless (test-andmap 'andmap andmap)
  (printf "fail: (test-andmap 'andmap andmap)~n"))

(define map1
  (lambda (p vs)
    (letrec ([visit (lambda (ws)
                      (cond
                        [(null? ws)
                         '()]
                        [(pair? ws)
                         (cons (p (car ws))
                               (visit (cdr ws)))]
                        [else
                         (errorf 'map1
                                 "not a proper list: ~s"
                                 ws)]))])
      (if (procedure? p)
          (visit vs)
          (errorf 'map1
                  "not a procedure: ~s"
                  p)))))

(define andmap1
  (lambda (p vs)
    (letrec ([visit (lambda (ws)
                      (if (null? ws)
                          #t
                          (and (p (car ws))
                               (visit (cdr ws)))))])
      (visit vs))))

(define our-very-own-andmap
  (lambda (p arg . args)
    (letrec ([visit (lambda (v vs)
                      (cond
                        [(null? v)
                         (if (andmap1 null? vs)
                             #t
                             (errorf 'our-very-own-andmap
                                     "input lists differ in length"))]
                        [(pair? v)
                         (if (andmap1 pair? vs)
                             (if (and (null? (cdr v))
                                      (andmap1 null? (map cdr vs)))
                                 (apply p (cons (car v) (map1 car vs)))
                                 (and (apply p (cons (car v) (map1 car vs)))
                                      (visit (cdr v) (map1 cdr vs))))
                             (errorf 'our-very-own-andmap
                                     "not a proper list: ~s"
                                     args))]
                        [else
                         (errorf 'our-very-own-andmap
                                 "not a proper list: ~s"
                                 arg)]))])
      (if (procedure? p)
          (visit arg args)
          (errorf 'our-very-own-andmap
                  "not a procedure: ~s"
                  p)))))


;;; Uncomment the following two lines:
(unless (test-andmap 'our-very-own-andmap our-very-own-andmap)
  (printf "fail: (test-andmap 'our-very-own-andmap our-very-own-andmap)~n"))

;;; andmap with fold-right

(define fold-right_proper-list
  (lambda (nil-case cons-case)
    (lambda (vs)
      (letrec ([visit (lambda (ws)
                        (if (null? ws)
                            nil-case
                            (cons-case (car ws)
                                       (visit (cdr ws)))))])
        (visit vs)))))

;;; We don't know how to do this

;;;;;;;;;;

;;; Exercise 2
;;; ----------
;;; 
;;; Redefine ``and-all`` using ``andmap``.
;;; 
;;; * Hint: remember to use a unit-test procedure.

(define and-all_alt
  (lambda bs_init
    (if (our-very-own-andmap (lambda (x) x) bs_init)
        #t
        #f)))

;; Uncomment the following two lines:
(unless (test-and-all 'and-all_alt and-all_alt)
  (printf "fail: (test-and-all 'and-all_alt and-all_alt)~n"))

;;;;;;;;;;

"week-02_unit-test-paraphernalia.scm"

;;; end of week-02_unit-test-paraphernalia.scm
