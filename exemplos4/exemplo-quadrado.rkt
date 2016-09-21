;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exemplo-quadrado) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; PROBLEMA: CRIAR UM PROGRAMA MUNDO QUE COMEÇA
;; COM UM QUADRADO VERMELHO MINUSCULO (TAMANHO 0) QUE VAI
;; AUMENTANDO INDEFINIDAMENTE. QUANDO APERTAR ESPAÇO
;; VOLTA AO TAMANHO ORIGINAL

(require 2htdp/image)
(require 2htdp/universe)

;; Meu programa DO QUADRADO VERMELHO

;; =================
;; Constantes:

(define LARGURA 1000)
(define ALTURA 1000)
(define CENARIO (empty-scene LARGURA ALTURA))
(define X (/ LARGURA 2))
(define Y (/ ALTURA 2))
(define COR "red")
(define VELOCIDADE-AUMENTO 3)

;; =================
;; Definições de dados:

;; Quadrado é Numero[0,inf+]
;; interp. um numero que representa o tamanho dos lados do quadrado
;exemplos:
(define INICIAL 0)
(define MAIS-OU-MENOS (/ LARGURA 2))
(define GRANDAO LARGURA)

#;
(define (fn-para-quadrado q)
  (.. q))



;; =================
;; Funções:

;; Quadrado -> Quadrado
;; inicie o mundo com (main 0)
;; 

(define (main q)
  (big-bang q                 ; Quadrado   (estado inicial do mundo)
            (on-tick aumentar)     ; Quadrado -> Quadrado    
                                   
            (to-draw desenha-quadrado)   ; Quadrado -> Image   
                                         
            (on-key    trata-tecla)   ; Quadrado KeyEvent -> Quadrado

            (on-mouse trata-mouse)  ; Quadrado Numero Numero MouseEvent -> Quadrado

            ))    
                                   
;; aumentar : Quadrado -> Quadrado
;; devolver um tamanho do quadrado aumentado com VELOCIDADE-AUMENTO
;(define (aumentar q) q)

(define (aumentar q)
  (+ q VELOCIDADE-AUMENTO))

;exemplos/testes
(check-expect (aumentar 0) (+ 0 VELOCIDADE-AUMENTO))
(check-expect (aumentar MAIS-OU-MENOS) (+ MAIS-OU-MENOS VELOCIDADE-AUMENTO)) 



;; desenha-quadrado: Quadrado -> Image
;; desenha quadrado na tela
;(define (desenha-quadrado q) CENARIO)

(define (desenha-quadrado q)
  (place-image (square q "solid" COR) X Y CENARIO))

;exemplos
(check-expect (desenha-quadrado 0) CENARIO)
(check-expect (desenha-quadrado MAIS-OU-MENOS)
              (place-image (square MAIS-OU-MENOS "solid" COR) X Y CENARIO))
              
              


;; trata-tecla: Quadrado KeyEvent -> Quadrado
;; quando receber espaço retorna ao estado 0
;(define (trata-tecla q ke) q)

(define (trata-tecla q ke)
  (cond [(key=? ke " ") 0]
        [else q]))

;exemplos
(check-expect (trata-tecla MAIS-OU-MENOS " ") 0)
(check-expect (trata-tecla MAIS-OU-MENOS "Q") MAIS-OU-MENOS)


;; Quadrado Integer Integer MouseEvent -> Quadrado
;; Quando clicar nas posições x y no mouse produz quadrado...   <apagar caso não precise usar>
;(define (trata-mouse q x y me) q)
;;!!!
(define (trata-mouse q x y me)
(cond [(mouse=? me "button-down")
       (* (- (/ LARGURA 2) x) 2)]
      [else   q]))

(check-expect (trata-mouse 50 100 500 "button-down")
              (* (- (/ LARGURA 2) 100) 2)) ;quando x < LARGURA/2


