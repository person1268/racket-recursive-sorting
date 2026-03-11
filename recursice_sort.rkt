#lang racket

(define (lastOut lst) ;pops out the last value in lst
  (reverse (cdr (reverse lst))))

(define (last1 lst) ;returns the last value in lst
  (if (null? (cdr lst)) ;if no items exist after potential last item
      (car lst) ;then return last item
      (last1 (cdr lst)))) ;otherwise, remove first item and recursively call last1

(define (mySort left right value counter)
  (if (= counter 0) ;if counter = 0
      (append left (list value)) ;then return sorted list
      (if (null? left) ;otherwise, check to see if left is empty
          (let ([_ (printf "~v\n" (cons value right))]) ;if left is empty, print right
            (mySort (lastOut (cons value right)) empty (last1 (cons value right)) (- counter 1))) ;then recursively call mySort with decremented counter
          (let* ([size (- (length left) 1)] ;if left is not empty, declare size, leftValue, and newLeft
                 [leftValue (list-ref left size)]
                 [newLeft (lastOut left)])
            (if (< leftValue value) ;if leftValue less than value
                ;recursively call mySort, passing value to right and replacing value with leftValue
                (mySort newLeft (cons value right) leftValue counter)
                ;otherwise
                ;recursively call mySort, passing leftValue into right
                (mySort newLeft (cons leftValue right) value counter))))))

(define lst '(20 13 74 5 12 9 22 94 22 6 96 72 3 53 33 22 21 101 3 17 15 95 88))

(define value (last1 lst))
(define left (lastOut lst))
(printf "Final result: ~v\n" (mySort left empty value (length lst)))