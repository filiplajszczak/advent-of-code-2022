#!/usr/bin/guile -s
!#

(use-modules ((srfi srfi-1) #:select (take take-right zip))
             ((srfi srfi-64) #:select (test-begin
                                       test-end
                                       test-equal))
             ((f) #:select (read-lines))
             ((algorithms) #:select (flatten product)))

(define (char->number chr)
  (- (char->integer chr) 48))

(define (check-rows rows columns rows-i columns-i proc)
  (let iter-rows ([remaining-rows rows] [row-index 0])
    (if (null? remaining-rows)
        '()
        (cons (proc (car remaining-rows) rows columns rows-i columns-i row-index)
              (iter-rows (cdr remaining-rows) (+ 1 row-index))))))

(define (zero-max . x)
  (if (null? x) -1 (apply max x)))

(define (directions rows columns tree-index columns-i rows-i row-index part)
  (let ([proc (if (eq? part 'scenic) reverse identity)])
    (list
     (proc (take (list-ref rows row-index) tree-index))
     (take-right (list-ref rows row-index) (- columns-i tree-index 1))
     (proc (take (list-ref columns tree-index) row-index))
     (take-right (list-ref columns tree-index) (- rows-i row-index 1)))))

(define (check-columns-visible row rows columns rows-i columns-i row-index)
  (let iter-trees ([row row] [tree-index 0])
    (let ([tree-ok?
           (λ () (pair?
                  (filter (λ (side) (> (car row) (apply zero-max side)))
                          (directions rows columns tree-index columns-i rows-i row-index 'visible))))])
      (cond [(null? row) '()]
            [(tree-ok?)
             (cons #t (iter-trees (cdr row) (+ 1 tree-index)))]
            [else (iter-trees (cdr row) (+ 1 tree-index))]))))

(define (calculate-score side row)
  (let iter ([side side] [acc 0])
    (cond [(null? side) acc]
          [(>= (car side) (car row)) (+ 1 acc)]
          [else (iter (cdr side) (+ 1 acc))])))

(define (check-columns-scenic row rows columns rows-i columns-i row-index)
  (let iter-trees ([row row][tree-index 0])
    (let ([scenic-score (λ ()
                          (product
                           (map
                            (λ (side) (calculate-score side row))
                            (directions rows columns tree-index columns-i rows-i row-index 'scenic))))])
      (if (null? row)
          '()
          (cons (scenic-score) (iter-trees (cdr row) (+ 1 tree-index)))))))

(define (solve-visible rows)
  (let* ([columns (apply zip rows)]
         [rows-i (length rows)]
         [columns-i (length columns)])
    (length (flatten (check-rows rows columns rows-i columns-i check-columns-visible)))))

(define (solve-scenic rows)
  (let* ([columns (apply zip rows)]
         [rows-i (length rows)]
         [columns-i (length columns)])
    (apply max (flatten (check-rows rows columns rows-i columns-i check-columns-scenic)))))

(define (part-1 filename)
  (solve-visible (input filename)))

(define (part-2 filename)
  (solve-scenic (input filename)))

(define (input filename)
  (map
   (λ (chars) (map char->number chars))
   (map string->list
        (read-lines filename))))


;; Part 1
(display (part-1 "day08.in"))
(newline)
;; Part 2
(display (part-2 "day08.in"))
(newline)

;; Tests
(test-begin "example")

(test-equal "Test part 1" 21 (part-1 "day08-example.in"))
(test-equal "Test part 2" 8 (part-2 "day08-example.in"))

(test-end "example")
