#!/usr/bin/guile -s
!#

(use-modules ((ice-9 match) #:select (match))
             ((srfi srfi-64) #:select (test-begin
                                       test-end
                                       test-equal))
             ((f) #:select (read-lines))
             ((algorithms) #:select (sum)))

(define (part-1 filename)
  (sum (map score (input filename))))

(define (part-2 filename)
  (sum (map strategy (input filename))))

(define (input filename)
   (read-lines filename))

(define (score line)
  (match line
    ["A X" 4]
    ["A Y" 8]
    ["A Z" 3]
    ["B X" 1]
    ["B Y" 5]
    ["B Z" 9]
    ["C X" 7]
    ["C Y" 2]
    ["C Z" 6]))

(define (strategy line)
  (match line
    ["A X" 3]
    ["A Y" 4]
    ["A Z" 8]
    ["B X" 1]
    ["B Y" 5]
    ["B Z" 9]
    ["C X" 2]
    ["C Y" 6]
    ["C Z" 7]))

;; Part 1
(display (part-1 "day02.in"))
(newline)
;; Part 2
(display (part-2 "day02.in"))
(newline)

;; Tests
(test-begin "example")

(test-equal "Test part 1" 15 (part-1 "day02-example.in"))
(test-equal "Test part 2" 12 (part-2 "day02-example.in"))

(test-end "example")
