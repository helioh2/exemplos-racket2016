;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname compostos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(define-struct pos (x y))   ;pos é o nome da estrutura. x e y são campos

(define N1 1)

(define P1 (make-pos 10 20))
(make-pos 0 0)  ; construtor

(define xp1 (pos-x P1))
(define yp1 (pos-y P1))   ;acessadores de campos

(pos? P1)   ;verificador



(define-struct pessoa (nome cpf telefone))

(define pessoa1 (make-pessoa "beltrano" "282828" "99899"))

(pessoa-nome pessoa1)
(pessoa-cpf pessoa1)
(pessoa-telefone pessoa1)





