#lang racket

(require rackunit)
;;----------------------
(define (fat n)
  (foldr * 1 (range 1 (add1 n))))
;significado: pega o intervalo de números entre 1 e n (função range)
;e aplica a função * em cada item do intervalo, começando com 1. Ex: 1*5*4*3*2*1 para n = 5

(check-equal? (fat 5) 120)
;;-----------------

(define (somatorio ldn)
  (foldr + 0 ldn))
;significado: aplica a função + em cada item da lista, começando com 0. Ex: 0+1+2+3

(check-equal? (somatorio '(1 2 3)) 6)
;;--------------------

(define (remove index lst)
  (append (take lst index) (drop lst (add1 index))))
;significado: take pega sublista desde o primeiro até o index-1,
; drop pega sublista desde o index+1 até o ultimo. Usa append para concatenar
; as sublistas resultantes de take e drop, resultando na lista sem o item no index.

(check-equal? (remove 3 '(1 2 3 4 5 6)) '(1 2 3 5 6))
(check-equal? (remove 5 '(1 2 3 4 5 6)) '(1 2 3 4 5))
(check-equal? (remove 0 '(1 2 3 4 5 6)) '(2 3 4 5 6))
;;--------------------

(define (index-of lst x)
  (for/or ([y lst] [i (in-naturals)] #:when (equal? x y)) i))
;significado: para cada y na lista lst, e para cada i no conjunto de numeros naturais (0,1,2,...),
;quando x for igual a y, retorna i

(check-equal? (index-of '(1 2 3 4 5 6) 5) 4)
(check-false (index-of '(1 2 3 4 5 6) 0))
;;--------------------

(define (replace old new lst)
  (let ([indice (index-of lst old)])
    (unless (false? indice)
      (list-set lst indice new))))
;significado: substituir a primeira ocorrência de old por new na lista lst.
; Para isso: seja indice o indice (posicao) de old na lst. A menos que ("unless") o índice seja
; falso (não encontrou), atribua o item na posicao indice para new.

(check-equal? (replace 5 10 '(1 2 3 4 5 6)) '(1 2 3 4 10 6))
;;--------------------

(define (replace-all old new lst)
  (map (lambda (item) (if (equal? item old) new item)) lst))
; significado: para cada item na lista, se o item é igual a old, coloca o new,
; senão, deixa como está (item).


(check-equal? (replace-all 5 10 '(1 2 3 4 5 6 5 3 2 5 3 2 6 5))
              '(1 2 3 4 10 6 10 3 2 10 3 2 6 10))