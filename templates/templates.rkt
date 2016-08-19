#lang racket

;;;;; Enciclopédia de templates (data driven templates e de interações big-bang)

;;;;;;; Dados atômicos (Numero, String, Imagem, Boolean, etc)

;; Numero
#;
(define (fn-para-numero x)
  (... x))

;; String
#;
(define (fn-para-string x)
  (... x))


;; Tempo é Natural
;; interp. número de ticks de clock desde o inicio do jogo
(define TEMPO-INICIO 0)
(define TEMPO-ATUAL 1000)
#;
(define (fn-para-tempo t)
  (... t))
;; Regras de template utilizadas:
;; - atomicos: Natural


;; Gato é Numero
;; interp. coordenada x do gato
(define G1 0)
(define G2 500)
#;
(define (fn-para-gato g)
  (... g))



;;;; Intervalos

;; ContagemRegressiva é Inteiro[0, 10]
;; interp. o número de segundos faltantes
(define C1 10) ; inicio
(define C2 5) ; meio
(define C3 0) ; fim
#;
(define (fn-para-contagem-regressiva cd)
  (... cd))


;;;;; Enumerações ("um dos possiveis valores")

;; CorSemaforo é um dos possiveis valores:
;; - "vermelho"
;; - "amarelo"
;; - "verde"
;; interp. a cor do semaforo
;;
#;
(define (fn-para-cor-semaforo cs)
  (cond [(string=? "vermelho" cs) (...)]
        [(string=? "amarelo" cs) (...)]
        [(string=? "verde" cs) (...)]))
;; Regras de template utilizadas:
;; - um dos possiveis valores: 3 casos
;; - atomico distinto: "vermelho"
;; - atomico distinto: "amarelo"
;; - atomico distinto: "verde"



;;;;; Itemizações (ou união, "um dos tipos")

;; Passaro é um dos tipos:
;;  - false
;;  - Numero
;; interp. false significa que não tem um passaro na tela,
;; e numero é a posicao x do passaro se ele existe
(define P1 false)
(define P2 3) 
#;
(define (fn-para-passaro p)
  (cond [(false? p) (...)]
        [(number? p) (... p)]))
;; Regras de template utilizadas:
;;  - um dos tipos: 2 casos
;;  - atomico distinto: false
;;  - atomic não-distinto: Numero



;;;;;;;; Itemização de intervalos

;;; Proximidade é um dos:
;;   - Numero[> 30]
;;   - Numero(5, 30]
;;   - Numero[0, 5]
;; interp. distancia em centimetros entre foguete e obstáculo 
;;     Numero[> 30] é considerado "seguro"
;;     Numero(5, 30] é considerado "cuidado"
;;     Numero[0, 5] é considerado "perigo"
(define PR1 40)
(define PR2 .9)
#;
(define (fn-para-proximidade p)
  (cond [(< 30 p) (... p)]
        [(and (< 5 p) (<= p 30)) (... p)]
        [(<= 0 p 5) (... p)]))

;; Regras de template utilizadas:
;; um dos: 3 casos
;; atomico não-distinto: Numero[>30]
;; atomic não-distinto: Numero(5, 30]
;; atomic não-distinto: Numero[0, 5]




; Listas dinâmicas (auto-referência, ou head-tail)

; ListaDeString é um desses:
;; - empty           ""(lista vazia)""
;; - (cons String ListaDeString)    ""uma String concatenada a uma ListaDeString""
;; interp. uma lista de strings
(define LDS-1 empty)
(define LDS-2 (cons "a" empty))
(define LDS-3 (cons "b" (cons "c" empty)))

#;
(define (fn-para-lds lds)
  (cond [(empty? lds) (...)]                   ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first lds)                 ;String
                   (fn-for-lds (rest lds)))])) ;RECURSÃO EM CAUDA

;; Regras de template utilizadas:
;; - um dos: 2 casos
;; - atomico distinto: empty
;; - composto: (cons String ListaDeString)
;; - auto-referencia: (rest lds) é ListaDeString




;;;;; Templates de interação big-bang

;; EstadoMundo KeyEvent -> EstadoMundo
#;
(define (trata-tecla estado ke)
  (cond [(key=? ke " ") (... estado)]
        [else
         (... estado)]))


;; EstadoMundo Integer Integer MouseEvent -> EstadoMundo
;; Muda o estado do mundo de acordo com 
;;    - o estado do mundo atual (EstadoMundo)
;;    - dois inteiros representado posição x e y do mouse (Inteiro Inteiro)
;;    - tipo de evento do mouse (MouseEvent). Exemplo: "button-down"
#;
(define (trata-mouse estado x y me)
  (cond [(mouse=? me "button-down") (... estado x y)]
      [else
       (... estado x y)]))