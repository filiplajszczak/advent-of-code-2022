#!/usr/bin/guile -s
!#

(use-modules (srfi srfi-1)
             (srfi srfi-64)
             ((f))
             ((algorithms)))

(define (parse-line line)
  (map (lambda (range) (map string->number (string-split range #\-)))(string-split line #\,)))

(define (contains? line)
  (let ([selections (parse-line line)])
    (or (and
         (>= (first (first selections)) (first (last selections)))
         (<= (last (first selections)) (last (last selections))))
        (and
         (<= (first (first selections)) (first (last selections)))
         (>= (last (first selections)) (last (last selections)))))))

(define (overlaps? line)
  (let ([selections (parse-line line)])
    (or (and
         (>= (first (first selections)) (first (last selections)))
         (<= (first (first selections)) (last (last selections))))
        (and
         (<= (last (first selections)) (last (last selections)))
         (>= (last (first selections)) (first (last selections))))
        (contains? line))))

(define (part-1 filename)
  (length (filter contains? (input filename))))

(define (part-2 filename)
  (length (filter overlaps? (input filename))))

(define (input filename)
   (read-lines filename))


;; Part 1
(display (part-1 "day04.in"))
(newline)
;; Part 2
(display (part-2 "day04.in"))
(newline)

;; Tests
(test-begin "example")

(test-equal "Test part 1" 2 (part-1 "day04-example.in"))
(test-equal "Test part 2" 4 (part-2 "day04-example.in"))

(test-end "example")
