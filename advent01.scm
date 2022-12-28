#!/usr/bin/guile -s
!#

(use-modules ((srfi srfi-1) #:select (take))
             ((srfi srfi-64) #:select (test-begin
                                       test-end
                                       test-equal))
             ((f) #:select (read-text))
             ((algorithms) #:select (sum)))

(define (part-1 filename)
  (apply max (map sum (input filename))))

(define (part-2 filename)
  (sum (take (sort (map sum (input filename)) >) 3)))

(define (input filename)
  (map
   (λ (str) (map string->number (string-split str #\newline)))
   (string->paragraphs (string-trim-both (read-text filename)))))

(define (string->paragraphs str)
  (let ((paragraph-break (string-contains str "\n\n")))
    (if paragraph-break
        (cons
         (string-take str paragraph-break)
         (string->paragraphs (string-drop str (+ 2 paragraph-break))))
        (cons str '()))))

;; Part 1
(display (part-1 "day01.in"))
(newline)
;; Part 2
(display (part-2 "day01.in"))
(newline)

;; Tests
(test-begin "example")

(test-equal "Test part 1"  24000 (part-1 "day01-example.in"))
(test-equal "Test part 2" 45000 (part-2 "day01-example.in"))

(test-end "example")
