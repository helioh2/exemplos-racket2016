;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname image-area-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;;PROBLEM:
;;
;;PROJETE uma função chamada 'area-imagem' que consome uma imagem e produz
;;a area de uma imagem. SIGA A RECEITA.

;; Imagem -> Numero
;; Calcular a área da imagem dada

;(define (area-imagem img) 0)  ;stub (prototipo inutil)

(define (area-imagem img)
   (* (image-width img) (image-height img)))

;;exemplos
(check-expect (area-imagem (square 50 "outline" "red"))
              (* 50 50))
(check-expect (area-imagem (rectangle 30 40 "solid" "blue"))
              (* 30 40))
(check-expect (area-imagem (circle 100 "solid" "blue"))
              (* 200 200))