#lang racket

(require 2htdp/image)
(require 2htdp/universe)

;; Meu programa mundo  (torne isto mais específico)

;; =================
;; Constantes:


;; =================
;; Definições de dados:

;; EstadoMundo é ... (dê um nome melhor para EstadoMundo)



;; =================
;; Funções:

;; EstadoMundo -> EstadoMundo
;; inicie o mundo com ...
;; 
#;
(define (main estado)
  (big-bang estado               ; EstadoMundo   (estado inicial do mundo)
            (on-tick   tock)     ; EstadoMundo -> EstadoMundo    
                                   ;(retorna um novo estado do mundo dado o atual a cada tick do clock)
            (to-draw   desenha-mundo)   ; EstadoMundo -> Image   
                                          ;(retorna uma imagem que representa o estado atual do mundo)
            (stop-when ...)      ; EstadoMundo -> Boolean    
                                    ;(retorna true se o programa deve terminar e false se deve continuar)
            (on-mouse  ...)      ; EstadoMundo Integer Integer MouseEvent -> EstadoMundo    
                                    ;(retorna um novo estado do mundo dado o estado atual e uma interação com o mouse)
            (on-key    ...)))    ; EstadoMundo KeyEvent -> EstadoMundo
                                    ;(retorna um novo estado do mundo dado o estado atual e uma interação com o teclado)

;; EstadoMundo -> EstadoMundo
;; produz o próximo ...
;; !!!
#;
(define (tock estado) ...)


;; EstadoMundo -> Image
;; desenha 
;; !!!
#;
(define (desenha-mundo estado) ...)


;; EstadoMundo KeyEvent -> EstadoMundo
;; quando teclar ...  produz ...  <apagar caso não precise usar>
#;
(define (handle-key estado ke)
  (cond [(key=? ke " ") (... estado)]
        [else
         (... estado)]))

;; EstadoMundo Integer Integer MouseEvent -> EstadoMundo
;; Quando fazer ... nas posições x y no mouse produz ...   <apagar caso não precise usar>
#;
(define (handle-mouse estado x y me)
(cond [(mouse=? me "button-down") (... estado x y)]
      [else
       (... estado x y)]))