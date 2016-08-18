;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname tabuleiro) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

;; (square <lado> <modo> <cor>)
;; (beside <img1> <img2> <img3> ...)
;; (above <img1> <img2> <img3> ...)


(define (linha-quadrados tamanho cor1 cor2)
  (beside  (square tamanho "solid" cor1)
           (square tamanho "solid" cor2)))


;; Numero -> Imagem
(define (padrao-xadrez tamanho cor1 cor2)
  (above (linha-quadrados (/ tamanho 2) cor1 cor2)
         (linha-quadrados (/ tamanho 2) cor2 cor1))
  )

;(padrao-xadrez 100)


(define (duplica img)
  (beside img img))

(define (quatro img)
  (above (duplica img)
         (duplica img)))

;; Imagem -> Imagem
(define (dezesseis img)
  (quatro (quatro img)))


(define (desenha-tabuleiro tamanho cor1 cor2)
  (dezesseis (padrao-xadrez (/ tamanho 4) cor1 cor2)))

(desenha-tabuleiro 200 "white" "blue")
         