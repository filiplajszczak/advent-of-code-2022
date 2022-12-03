#!/usr/bin/guile -s
!#

(use-modules (srfi srfi-1)
             (srfi srfi-64)
             ((f))
             ((algorithms)))

(define (halve lst)
  (chunks-of lst (/ (length lst) 2)))

(define (intersection lst)
  (first (char-set->list (char-set-intersection (list->char-set (first lst)) (list->char-set (last lst))))))

(define (char->priority chr)
  (if (char-lower-case? chr)
      (- (char->integer chr) 96)
      (- (char->integer chr) 38)))

(define (part-1 filename)
  (sum (map
        (compose char->priority intersection halve string->list)
        (input filename))))

(define (part-2 filename)
  (sum (map
        (compose char->priority first char-set->list (lambda (lst) (apply char-set-intersection lst)))
        (chunks-of (map
                    (compose list->char-set string->list)
                    (input filename))
                   3))))

(define (input filename)
   (read-lines filename))


;; Part 1
(display (part-1 "day03.in"))
(newline)
;; Part 2
(display (part-2 "day03.in"))
(newline)

;; Tests
(test-begin "example")

(test-equal "Test part 1" 157 (part-1 "day03-example.in"))
(test-equal "Test part 2" 70 (part-2 "day03-example.in"))

(test-end "example")
