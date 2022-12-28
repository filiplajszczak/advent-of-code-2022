#!/usr/bin/guile -s
!#

(use-modules ((srfi srfi-64) #:select (test-begin
                                       test-end
                                       test-equal))
             ((f) #:select (read-text))
             ((algorithms) #:select (sliding)))

(define (scan lst index size)
  (if (eq? (char-set-size (list->char-set (car lst))) size)
      index
      (scan (cdr lst) (+ index 1) size)))

(define (solve data size)
  (scan (sliding (string->list data) size 1) size size))

(define (part-1 filename)
  (solve (input filename) 4))

(define (part-2 filename)
  (solve (input filename) 14))

(define (input filename)
  (string-trim-right (read-text filename)))


;; Part 1
(display (part-1 "day06.in"))
(newline)
;; Part 2
(display (part-2 "day06.in"))
(newline)

;; Tests
(test-begin "example")

(test-equal "Test part 1-1" 7 (part-1 "day06-example-1.in"))
(test-equal "Test part 1-2" 5 (part-1 "day06-example-2.in"))
(test-equal "Test part 1-3" 6 (part-1 "day06-example-3.in"))
(test-equal "Test part 1-4" 10 (part-1 "day06-example-4.in"))
(test-equal "Test part 1-5" 11 (part-1 "day06-example-5.in"))
(test-equal "Test part 1-1" 19 (part-2 "day06-example-1.in"))
(test-equal "Test part 1-2" 23 (part-2 "day06-example-2.in"))
(test-equal "Test part 1-3" 23 (part-2 "day06-example-3.in"))
(test-equal "Test part 1-4" 29 (part-2 "day06-example-4.in"))
(test-equal "Test part 1-5" 26 (part-2 "day06-example-5.in"))

(test-end "example")
