;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname revisao) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define V1 0)

(define V2 10)

(define V3 "N")

(define N 20)

(define V4 N)

;; Numero Numero -> Numero
(define (soma n1 n2)
  (+ n1 n2))

;;+ : Numero Numero ... -> Numero
(+ 1 2 3.5)

;; - : Numero Numero ... -> Numero
(- 10 2 1)

;; = : Numero Numero ... -> Boolean
(= 4 4 4)
(= 2 3 4)

;; >= : Numero Numero ... -> Boolean
(>= 3 2 1)

;; string=? : String String ... -> Boolean
(string=? "fulano" "fulanO")

;; string-append : String String ... -> String
(string-append "hello" "world")

;; rectangle : Numero Numero String String -> Image
(rectangle 20 30 "solid" "red")

;; image-height : Image -> Numero
(image-height (rectangle 20 30 "solid" "red"))


;; overlay : Image Image ... -> Image
(overlay (rectangle 20 30 "solid" "red") (circle 100 "solid" "blue"))


