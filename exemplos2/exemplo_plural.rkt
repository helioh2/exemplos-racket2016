;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exemplo_plural) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;PROBLEMA:
;; Criar uma função que faz uma palavra ficar no plural
;; (por simplicidade, consideremos que basta adicionar 's' ao final da palavra)

;; pluralizar: String -> String
;; deixar a palavra dada no plural (com 's' no final)

(define (pluralizar p) 
  (string-append p "s"))

;; Exemplos (testes)
(check-expect (pluralizar "aluno") (string-append "aluno" "s"))
(check-expect (pluralizar "casa") (string-append "casa" "s"))
