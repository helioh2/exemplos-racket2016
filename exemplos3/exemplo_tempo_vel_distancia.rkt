;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exemplo_tempo_vel_distancia) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;;PROBLEMA: fazer uma função para calcular a distancia (em m)
;; com base na velocidade (em m/s) e tempo (em s)



;; Distancia é um Numero
;; interp. é um número que representa a distância em metros

(define D1 50)  ;; 50 metros
(define D2 100) ;; 100 metros

#;
(define (fn-para-distancia d)
  (... d))



;; Velocidade é um Numero
;; interp. é um número que representa a velocidade em metros por segundo (m/s)

(define V1 20)  ; 20 m/s
(define V2 -10) ; -10 m/s

#;
(define (fn-para-velocidade v)
  (... v))


;; Tempo é Natural
;; interp. é um número que indica um intervalo de tempo em segundos
;; sem ser em décimos de segundo

(define T1 0)   ; t=0
(define T2 10) ; t=10

#;
(define (fn-para-tempo t)
  (... t))


;; Velocidade Tempo -> Distancia
;; calcula a distancia com base na velocidade e tempo

;(define (calcula-distancia v t) 0)

(define (calcula-distancia v t)
  (* v t))

(check-expect (calcula-distancia 20 10) (* 20 10))
(check-expect (calcula-distancia -50 20) (* -50 20)) 






