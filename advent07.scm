#!/usr/bin/guile -s
!#

(use-modules ((srfi srfi-1) #:select (first last remove second))
             ((srfi srfi-64) #:select (test-begin
                                       test-end
                                       test-equal))
             ((f) #:select (read-lines))
             ((algorithms) #:select (init sum)))

(define (update-acc acc pwd size)
  (if (null? pwd)
      acc
      (update-acc
       (assoc-set! acc pwd (+ (assoc-ref acc pwd) size))
       (init pwd)
       size)))

(define (get-sizes lines acc pwd)
  (cond
   [(null? lines) (map cdr acc)]
   [(and
     (string=? (second (car lines)) "cd")
     (not (string=? (last (car lines)) "..")))
    (get-sizes
     (cdr lines)
     (assoc-set! acc (append pwd (list (last (car lines)))) 0)
     (append pwd (list (last (car lines)))))]
   [(and
     (string=? (second (car lines)) "cd")
     (string=? (last (car lines)) ".."))
    (get-sizes (cdr lines) acc (init pwd))]
   [else (get-sizes
          (cdr lines)
          (update-acc acc pwd (string->number (first (car lines))))
          pwd)]))

(define (part-1 filename)
  (sum (filter
        (λ (size) (< size 100000))
        (get-sizes (input filename) '() '()))))

(define (part-2 filename)
  (let* ([sizes (sort (get-sizes (input filename) '() '()) >)]
         [root (car sizes)]
         [empty-space (- 70000000 root)]
         [required (- 30000000 empty-space)])
  (last (filter (λ (size) (>= size required)) sizes))))

(define (input filename)
  (map
   (λ (line) (string-split line #\space))
   (remove
    (λ (line)
      (or (string=? "$ ls" line) (string-prefix? "dir" line)))
    (read-lines filename))))

;; Part 1
(display (part-1 "day07.in"))
(newline)
;; Part 2
(display (part-2 "day07.in"))
(newline)

;; Tests
(test-begin "example")

(test-equal "Test part 1" 95437 (part-1 "day07-example.in"))
(test-equal "Test part 2" 24933642 (part-2 "day07-example.in"))

(test-end "example")
