;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname vaca) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)


;; Programa da vaca na cerca

;;CONSTANTES:
(define LARGURA 600)
(define ALTURA 400)
(define CENARIO (rectangle LARGURA ALTURA "outline" "black"))
(define IMG-VACA-INO (bitmap "vaca-ino.png"))
(define IMG-VACA-VORTANO (flip-horizontal IMG-VACA-INO))
(define Y (/ ALTURA 2))

(define IMG-CC (scale 0.5 (bitmap "chupacabra.jpg")))

(define X-CC (/ LARGURA 2))
(define DY-CC-DEFAULT 5)

(define MEIO-H-VACA (/ (image-width IMG-VACA-INO) 2 ))
(define MEIO-H-CC (/ (image-width IMG-CC) 2 ))

(define LIMITE-DIREITO (- LARGURA MEIO-H-VACA))

(define TELA-GAME-OVER (overlay (text "GAME OVER" 30 "red") CENARIO))


;;DEFINIÇÕES DE DADOS
                           
(define-struct chupacabra (x dx y dy))
;; Chupacabra é (make-chupacabra Natural Inteiro Natural Inteiro)
;; interp. representa o chupacabra que está numa posição y
;; da tela e anda a uma velocidade dy (dy também indica a direção
;; em que ele está apontando)

;exemplos:
(define CC-INICIAL (make-chupacabra X-CC 0 0 DY-CC-DEFAULT))
(define CC-MEIO (make-chupacabra X-CC 0 (/ ALTURA 2) DY-CC-DEFAULT))
(define CC-ANTES-VIRAR (make-chupacabra X-CC 0 (+ ALTURA 5) DY-CC-DEFAULT))
(define CC-VIROU (make-chupacabra X-CC 0 ALTURA (- DY-CC-DEFAULT)))
(define CC-CHEGANDO (make-chupacabra X-CC 0 -5 (- DY-CC-DEFAULT)))
(define CC-VIROU-L-CIMA (make-chupacabra X-CC 0 0 DY-CC-DEFAULT))

#;
(define (fn-para-chupacabra cc)
  (... (chupacabra-y cc) (chupacabra-dy cc))
  )


(define-struct vaca (x dx))
;;Vaca é (make-vaca Natural Inteiro)
;;interp. representa a vaca que está numa posição x
;;da tela e anda a uma velocidade dx (dx também indica a direção
;;em que ela está apontando)

;exemplos:
(define VACA-INICIAL (make-vaca 0 10))
(define VACA-MEIO (make-vaca (/ LARGURA 2) 10))
(define VACA-ANTES-VIRAR (make-vaca (+ LIMITE-DIREITO 5) 10))
(define VACA-VIRADA (make-vaca LIMITE-DIREITO -10))
(define VACA-MEIO-VORTANO (make-vaca (/ LARGURA 2) -10))
(define VACA-CHEGANDO (make-vaca 50 -10))
(define VACA-ULTRAPASSOU (make-vaca (+ LIMITE-DIREITO 20) 50))
(define VACA-NO-LIMITE (make-vaca LIMITE-DIREITO -50))

#;
(define (fn-para-vaca v)
  (... (vaca-x v) (vaca-dx v))
  )


;; ListaDeChupacabra é um desses:
;; - empty       
;; - (cons Chupacabra ListaDeChupacabra)    
;; interp. uma lista de chupacabras
(define LDCC-1 empty)
(define LDCC-2 (cons CC-INICIAL empty))
(define LDCC-3 (cons CC-INICIAL (cons CC-MEIO empty)))

#;
(define (fn-para-ldcc ldcc)
  (cond [(empty? ldcc) (...)]                   ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldcc)                 ;Chupacabra
                   (fn-for-ldcc (rest ldcc)))])) ;RECURSÃO EM CAUDA


(define-struct jogo (vaca chupacabras game-over?))
;; Jogo é (make-jogo Vaca ListaDeChupacabra Boolean)
;; interp. representa um jogo que tem uma vaca
;; e VARIOS chupacabra.

(define JOGO-INICIAL (make-jogo VACA-INICIAL
                                (list CC-INICIAL)
                                #false))
(define JOGO-MEIO (make-jogo VACA-ANTES-VIRAR
                                (list CC-MEIO)
                                #false))
(define JOGO-ZICA (make-jogo
                   (make-vaca (- (/ LARGURA 2) MEIO-H-CC -5) 10)
                   (list CC-MEIO)
                   #false))
(define JOGO-ZICA-BRABA (make-jogo
                   (make-vaca (- (/ LARGURA 2) MEIO-H-CC -5) 10)
                   (list CC-MEIO)
                   #true))
(define JOGO-ACABOU (make-jogo VACA-MEIO
                               (list CC-MEIO)
                               #true))

(define JOGO-3-CHUPACABRAS (make-jogo VACA-INICIAL
                                      (list CC-INICIAL
                                            (make-chupacabra
                                             (* LARGURA 0.25)
                                             0
                                             (/ ALTURA 2)
                                             DY-CC-DEFAULT)
                                            (make-chupacabra
                                             (* LARGURA 0.75)
                                             0
                                             (* ALTURA 0.25)
                                             DY-CC-DEFAULT))
                                      #false))

#;
(define (fn-para-jogo j)
  (... (jogo-vaca j)
       (jogo-chupacabras j)
       (jogo-game-over? j)))


;; =================
;; Funções:

;; Jogo -> Jogo
;; inicie o mundo com (main JOGO-3-CHUPACABRAS)

(define (main j)
  (big-bang j      ; Jogo
            (on-tick proximo-jogo)
            (to-draw desenha-jogo)
            (on-key trata-tecla)))



;; INICIO DA PARTE LÓGICA DO JOGO

;; proximo-jogo : Jogo -> Jogo
;; atualiza o jogo
;(define (proximo-jogo j)  j)

(define (proximo-jogo j)
  (cond [(colisao-vaca-algum-chupacabra? (jogo-vaca j) (jogo-chupacabras j))
         (make-jogo (jogo-vaca j)
                    (jogo-chupacabras j)
                    #true)]

        [else (make-jogo (proxima-vaca (jogo-vaca j))
                         (proximos-chupacabras (jogo-chupacabras j))
                         (jogo-game-over? j))]
  ))


;caso normal
(check-expect (proximo-jogo JOGO-INICIAL)
              (make-jogo (make-vaca 10 10)
                         (list
                          (make-chupacabra X-CC 0 DY-CC-DEFAULT DY-CC-DEFAULT))
                         #false))

(check-expect (proximo-jogo JOGO-ZICA)
              JOGO-ZICA-BRABA)


;; colisao-vaca-algum-chupacabra? : Vaca ListaDeChupacabra -> Boolean
;; verifica se vaca colidiu com algum dos chupacabras
;(define (colisao-vaca-algum-chupacabra? v ldcc) #false)

(define (colisao-vaca-algum-chupacabra? v ldcc)
  (cond [(empty? ldcc) #false]                   ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (or (colisao-vaca-chupacabra? v (first ldcc))                 ;Chupacabra
                   (colisao-vaca-algum-chupacabra? v (rest ldcc)))])) ;RECURSÃO EM CAUDA



(check-expect (colisao-vaca-algum-chupacabra? VACA-MEIO
                                              (list CC-MEIO
                                                    CC-INICIAL
                                                    CC-CHEGANDO))
              #true)
(check-expect (colisao-vaca-algum-chupacabra? VACA-INICIAL
                                              (list CC-MEIO
                                                    CC-INICIAL
                                                    CC-CHEGANDO))
              #false)
(check-expect (colisao-vaca-algum-chupacabra? VACA-INICIAL
                                              empty)
              #false)



;; proximos-chupacabras : ListaDeChupacabra -> ListaDeChupacabra
;; faz o proximo-chupacabra para cada chupacabra da lista
;(define (proximos-chupacabras ldcc) ldcc)

;(define (proximos-chupacabras ldcc)
;  (cond [(empty? ldcc) empty]                   ;CASO BASE (CONDIÇÃO DE PARADA)
;        [else (cons (proximo-chupacabra (first ldcc))                 ;Chupacabra
;                   (proximos-chupacabras (rest ldcc)))])) ;RECURSÃO EM CAUDA

(define (proximos-chupacabras ldcc)
  (map proximo-chupacabra ldcc))


;exemplos
(check-expect (proximos-chupacabras empty) empty)
(check-expect (proximos-chupacabras
               (list CC-ANTES-VIRAR
                     (make-chupacabra X-CC 0 0 10)))
              (list CC-VIROU
                    (make-chupacabra X-CC 0 10 10)))
                    


;; colisao-vaca-chupacabra? : Vaca Chupacabra -> Boolean
;; verifica se vaca e chupacabra trombaram
;!!!
;(define (colisao-vaca-chupacabra? v cc) #false)
(define (colisao-vaca-chupacabra? v cc)
  (<= (distancia (vaca-x v) Y
             (chupacabra-x cc) (chupacabra-y cc))
      (+ MEIO-H-VACA MEIO-H-CC)))


(check-expect (colisao-vaca-chupacabra?
               (make-vaca (- (/ LARGURA 2) MEIO-H-CC -5) 10)
               CC-MEIO)
              #true)
(check-expect (colisao-vaca-chupacabra?
               VACA-INICIAL
               CC-INICIAL)
              #false)

;; distancia : Numero Numero Numero Numero -> Numero
;; calcula distancia
; !!!
(define (distancia x1 y1 x2 y2)
  (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))

(check-expect (distancia 3 0 0 4) 5)

;; proximo-chupacabra : Chupacabra -> Chupacabra
;; faz chupacabra andar no eixo y, e se trombar nos limites,
;; inverte dy

(define (proximo-chupacabra cc)
  (cond 
        [(> (chupacabra-y cc) ALTURA)
         (make-chupacabra (chupacabra-x cc)
                          (chupacabra-dx cc)
                          ALTURA (- (chupacabra-dy cc)))]
        [(< (chupacabra-y cc) 0)
         (make-chupacabra (chupacabra-x cc)
                          (chupacabra-dx cc)
          0 (- (chupacabra-dy cc)))]
        [else
         (make-chupacabra (chupacabra-x cc)
                          (chupacabra-dx cc)
          (+ (chupacabra-y cc) (chupacabra-dy cc))
             (chupacabra-dy cc))])
 )


; exemplos / testes
;casos em que ela anda pra direita sem chegar no limite
(check-expect (proximo-chupacabra (make-chupacabra X-CC 0 0 10))
              (make-chupacabra X-CC 0 10 10))
(check-expect (proximo-chupacabra CC-MEIO)
              (make-chupacabra X-CC 0 (+ (/ ALTURA 2) DY-CC-DEFAULT)
                         DY-CC-DEFAULT))
; casos em que chega no limite direito e tem que ccirar
(check-expect (proximo-chupacabra CC-ANTES-VIRAR)
              CC-VIROU)

; caso em que ela anda pra esquerda sem chegar no limite 
(check-expect (proximo-chupacabra
               (make-chupacabra X-CC 0 (/ ALTURA 2) (- DY-CC-DEFAULT)))
              (make-chupacabra X-CC 0 (- (/ ALTURA 2) DY-CC-DEFAULT)
                                       (- DY-CC-DEFAULT)))

; casos em que chega no limite esquerdo e tem que virar
(check-expect (proximo-chupacabra (make-chupacabra X-CC 0 -10 -10))
                            (make-chupacabra X-CC 0 0 10))
(check-expect (proximo-chupacabra (make-chupacabra X-CC 0 -20 -50))
                            (make-chupacabra X-CC 0 0 50))

;; proxima-vaca : Vaca -> Vaca
;; recebe uma vaca na posicao x e retorna uma vaca com posição
;; x atualizada com o dx
;(define (proxima-vaca v) v)
(define (proxima-vaca v)
  (cond 
        [(> (vaca-x v) LIMITE-DIREITO)
         (make-vaca LIMITE-DIREITO (- (vaca-dx v)))]
        [(< (vaca-x v) 0)
         (make-vaca 0 (- (vaca-dx v)))]
        [else
         (make-vaca (+ (vaca-x v) (vaca-dx v))
             (vaca-dx v))])
 )


; exemplos / testes
;casos em que ela anda pra direita sem chegar no limite
(check-expect (proxima-vaca (make-vaca 0 10))
              (make-vaca 10 10))
(check-expect (proxima-vaca VACA-MEIO)
              (make-vaca (+ (/ LARGURA 2) 10)
                         10))
; casos em que chega no limite direito e tem que virar
(check-expect (proxima-vaca VACA-ANTES-VIRAR)
              VACA-VIRADA)
(check-expect (proxima-vaca VACA-ULTRAPASSOU)
                            VACA-NO-LIMITE)
; caso em que ela anda pra esquerda sem chegar no limite 
(check-expect (proxima-vaca VACA-MEIO-VORTANO)
                            (make-vaca (- (/ LARGURA 2) 10)
                                       -10))

; casos em que chega no limite esquerdo e tem que virar
(check-expect (proxima-vaca (make-vaca -10 -10))
                            (make-vaca 0 10))
(check-expect (proxima-vaca (make-vaca -20 -50))
                            (make-vaca 0 50))

              
;; FIM DA PARTE LÓGICA


;; INICIO DA PARTE VISUAL

;; desenha-jogo : Jogo -> Image
;; desenha o jogo
;!!!
(define (desenha-jogo j)
  (if (jogo-game-over? j) TELA-GAME-OVER
  (overlay
   (desenha-chupacabras (jogo-chupacabras j))
   (desenha-vaca (jogo-vaca j)))))


;; desenha-chupacabras : ListaDeChupacabra -> Image
;(define (desenha-chupacabras ldcc) ldcc)

(define (desenha-chupacabras ldcc)
  (cond [(empty? ldcc) CENARIO]                   ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (overlay (desenha-chupacabra (first ldcc))                ;Chupacabra
                   (desenha-chupacabras (rest ldcc)))])) ;RECURSÃO EM CAUDA


;; desenha-chupacabra : Chupacabra -> Image
;; desenha o chupacabra
(define (desenha-chupacabra cc)
  (place-image IMG-CC (chupacabra-x cc) (chupacabra-y cc) CENARIO))


;; desenha-vaca: Vaca -> Image
;; retorna a representação do cenário com a vaca
#;
(define (fn-para-vaca v)
  (... (vaca-x v) (vaca-dx v))
  )

(define (desenha-vaca v)
  (place-image
   (if (< (vaca-dx v) 0)
       IMG-VACA-VORTANO
       IMG-VACA-INO)
   (vaca-x v)
   Y
   CENARIO)               
  )


;; FIM DA PARTE VISUAL

;; INICIO DA LOGICA DE INTERAÇÃO

;; trata-tecla : Jogo KeyEvent -> Jogo
;; trata tecla usando trata-tecla-vaca
;!!!
(define (trata-tecla j ke)
  (cond
    [(and (jogo-game-over? j) (key=? ke "\r"))
         JOGO-3-CHUPACABRAS]
    [else (make-jogo
           (trata-tecla-vaca (jogo-vaca j) ke)
           (jogo-chupacabras j)
           (jogo-game-over? j)
           )]))


(check-expect (trata-tecla (make-jogo
                            (make-vaca 100 10)
                            CC-INICIAL #false) " ")
              (make-jogo
               (make-vaca 100 -10)
               CC-INICIAL #false))

(check-expect (trata-tecla JOGO-ZICA-BRABA "\r")
              JOGO-3-CHUPACABRAS)

;; trata-tecla-vaca: Vaca KeyEvent -> Vaca
;; quando tecla espaço é pressionada, vaca deve inverter direção (dx)
;(define (trata-tecla-vaca v ke) v)

(define (trata-tecla-vaca v ke)
  (cond [(key=? ke " ")
         (make-vaca (vaca-x v) (* (vaca-dx v) -1))]
        [else v]))

(check-expect (trata-tecla-vaca (make-vaca 100 10) " ")
              (make-vaca 100 -10))
(check-expect (trata-tecla-vaca (make-vaca 200 -10) " ")
              (make-vaca 200 10))
(check-expect (trata-tecla-vaca VACA-INICIAL "a")
              VACA-INICIAL)

;; FIM DA LOGICA DE INTERAÇÃO



;; CÓDIGO ANTIGO:

;; Vaca -> Vaca
;; inicie o mundo com (main-vaca VACA-INICIAL)
;; 

(define (main-vaca v)
  (big-bang v               ; Vaca   (estado inicial do mundo)
            (on-tick   proxima-vaca)     ; Vaca -> Vaca    
                                 
            (to-draw   desenha-vaca)   ; Vaca -> Image   
                                          
            (on-key    trata-tecla-vaca)))    ; Vaca KeyEvent -> Vaca


