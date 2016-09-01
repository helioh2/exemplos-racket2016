;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exemplo_dobro) (read-case-sensitive #t) (teachpacks ((lib "testing.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "testing.rkt" "teachpack" "htdp")) #f)))
;; PROBLEMA:
;; Projete uma função que consome um número e produz o numero vezes dois.
;; Vamos chamar essa função de 'dobro'. Vamos seguir a receita.


;; Numero -> Numero
;; propósito: recebe um numero e calcula o dobro

;(define (dobro n) 0)   ; stub (protótipo burro)

(define (dobro n)
  (+ n n))

;(dobro 5)  ;--> 10
(check-expect (dobro 5) (+ 5 5))
(check-expect (dobro -10) -20)
; (check-expect <chamada pra funcao> <valor esperado> )
