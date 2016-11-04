#lang racket

(require rackunit)
;;----------------------
(define (fat n)
  (foldr * 1 (range 1 (add1 n))))

(check-equal? (fat 5) 120)
;;-----------------

(define (somatorio ldn)
  (foldr + 0 ldn))

(check-equal? (somatorio '(1 2 3)) 6)
;;--------------------

(define (remove index lst)
  (append (take lst index) (drop lst (add1 index))))

(check-equal? (remove 3 '(1 2 3 4 5 6)) '(1 2 3 5 6))
(check-equal? (remove 5 '(1 2 3 4 5 6)) '(1 2 3 4 5))
(check-equal? (remove 0 '(1 2 3 4 5 6)) '(2 3 4 5 6))
;;--------------------

(define (index-of lst x)
  (for/or ([y lst] [i (in-naturals)] #:when (equal? x y)) i))

(check-equal? (index-of '(1 2 3 4 5 6) 5) 4)
(check-false (index-of '(1 2 3 4 5 6) 0))
;;--------------------

(define (replace old new lst)
  (let ([indice (index-of lst old)])
    (unless (false? indice)
      (list-set lst indice new))))

(check-equal? (replace 5 10 '(1 2 3 4 5 6)) '(1 2 3 4 10 6))
;;--------------------

(define (replace-all old new lst)
  (map (lambda (item) (if (equal? item old) new item)) lst))

(check-equal? (replace-all 5 10 '(1 2 3 4 5 6 5 3 2 5 3 2 6 5))
              '(1 2 3 4 10 6 10 3 2 10 3 2 6 10))