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


;;----------------------

(define (produto-cartesiano-v1 list1 list2)
  (local
    [
    (define (cria-pares item lista)
      (map (lambda (item2) (list item item2)) lista))
    ]
    (cond [(empty? list1) empty]
          [else
           (append (cria-pares (first list1) list2)
                   (produto-cartesiano-v1 (rest list1) list2))])))

(check-equal? (produto-cartesiano-v1 '("a" "b" "c") '(1 2))
              '(("a" 1) ("a" 2) ("b" 1) ("b" 2) ("c" 1) ("c" 2)))


(define (produto-cartesiano-v2 list1 list2)
  (for*/list ([i list1]
             [j list2])
    (list i j)))

(check-equal? (produto-cartesiano-v2 '("a" "b" "c") '(1 2))
              '(("a" 1) ("a" 2) ("b" 1) ("b" 2) ("c" 1) ("c" 2)))


(define (produto-cartesiano-v3 list1 list2)
  (cartesian-product list1 list2))

(check-equal? (produto-cartesiano-v3 '("a" "b" "c") '(1 2))
              '(("a" 1) ("a" 2) ("b" 1) ("b" 2) ("c" 1) ("c" 2)))


;-------------------------

(define (todas-combinacoes-string list1 list2)
  (local
    [
     (define (to-string x)
       (cond [(number? x) (number->string x)]
             [(symbol? x) (symbol->string x)]
             [(list? x) (list->string x)]
             [(boolean? x) (if (false? x) "false" "true")]
             [else x]))
     ]
  (map (lambda (par) (string-append (to-string (first par)) " "
                                    (to-string (second par))))
       (produto-cartesiano-v3 list1 list2))))

(check-equal? (todas-combinacoes-string '(1 3) '(2 4))
              '("1 2" "1 4" "3 2" "3 4"))

;--------------------

(define (soma-produto-cartesiano-e-multiplica list1 list2)
  (foldr + 0 (map (lambda (par) (+ (first par) (second par)))
                  (produto-cartesiano-v3 list1 list2))))