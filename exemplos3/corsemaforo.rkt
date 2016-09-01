;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname corsemaforo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; PROBLEMA: projete uma definição de dado para a cor atual de um
;; semaforo. Um semaforo pode ter 3 valores possiveis:
;; "vermelho", "verde", "amarelo"

;; EXEMPLO DE TIPO ENUMERADO:

;; CorSemaforo é um desses (só pode ser um dos possíveis valores):
;;  - "vermelho"
;;  - "amarelo"
;;  - "verde"
;; interp. representa a cor de um semáforo
;; dispensa exemplos

#;
(define (fn-para-cor-semaforo cs)
  (cond [(string=? cs "vermelho") (... cs) ]
        [(string=? cs "amarelo") (... cs)]
        [(string=? cs "verde") (... cs)]
        ))
;; REGRA DE TEMPLATE USADA: ENUMERAÇÃO


;; PROBLEMA: projete uma função que consome uma cor
;; do semáforo e retorna a próxima cor (próximo estado)
;; do semáforo.

;; CorSemaforo -> CorSemaforo
;; retorna a próxima cor do semaforo

;(define (proxima-cor cs) cs)

(define (proxima-cor cs)
  (cond [(string=? cs "vermelho") "verde"]
        [(string=? cs "amarelo") "vermelho"]
        [(string=? cs "verde") "amarelo"]
        ))

;template utilizado:
#; 
(define (fn-para-cor-semaforo cs)
  (cond [(string=? cs "vermelho") (... cs) ]
        [(string=? cs "amarelo") (... cs)]
        [(string=? cs "verde") (... cs)]
        ))

(check-expect (proxima-cor "vermelho") "verde")
(check-expect (proxima-cor "verde") "amarelo")
(check-expect (proxima-cor "amarelo") "vermelho")



