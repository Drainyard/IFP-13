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



(define our-very-own-andmap
  (lambda (p . args)
    (cond
      [(null? args)
       (errorf 'our-very-own-andmap
               "incorrect argument count: ~s"
               args)]
      ;; [(not (and (map list? args)))
      ;;  (errorf 'our-very-own-andmap
      ;;          "one argument is not a proper list: ~s"
      ;;          args)]
      [(not (apply = (map length args)))
       (errorf 'our-very-own-andmap
               "different input list lengths: ~s"
               args)]
      [else
       (letrec ([visit (lambda (as)
                         (cond
                           [(null? (car as))
                            #t]
                           [(null? (cdar as))
                            (apply p (map car as))]
                           [(pair? (car as))
                            (and (apply p (map car as))
                                 (visit (map cdr as)))]
                           [else
                            (errorf 'our-very-own-andmap
                                    "not a proper input list: ~s"
                                    as)]))])
         (visit args))]
      )))


;;; Uncomment the following two lines:
(unless (test-andmap 'our-very-own-andmap our-very-own-andmap)
  (printf "fail: (test-andmap 'our-very-own-andmap our-very-own-andmap)~n"))

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
