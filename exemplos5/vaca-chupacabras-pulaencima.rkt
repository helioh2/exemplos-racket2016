;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname vaca-chupacabras-pulaencima) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

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
(define MEIO-V-VACA (/ (image-height IMG-VACA-INO) 2 ))
(define MEIO-H-CC (/ (image-width IMG-CC) 2 ))
(define MEIO-V-CC (/ (image-height IMG-CC) 2 ))

(define LIMITE-DIREITO (- LARGURA MEIO-H-VACA))
(define LIMITE-ESQUERDO MEIO-H-VACA)
(define LIMITE-BAIXO (- ALTURA MEIO-V-VACA))
(define LIMITE-CIMA MEIO-V-VACA)

(define TELA-GAME-OVER (overlay (text "GAME OVER" 30 "red") CENARIO))

(define X-SPAWN (* LARGURA 0.75))
(define Y-SPAWN (* ALTURA 0.25))
(define T-SPAWN (* 20 2))

(define D-VACA-DEFAULT 10)

(define Y-CHAO (* ALTURA 0.75))
(define Y-CHAO-VACA (- Y-CHAO MEIO-V-VACA))
(define Y-CHAO-CC (- Y-CHAO MEIO-V-CC))
(define G (/ 30 28))
(define DY-PULO -20)
(define DY-QUICA -10)

;;DEFINIÇÕES DE DADOS
                            

(define-struct chupacabra (x dx y dy))
;; Chupacabra é (make-chupacabra Natural Natural Inteiro)
;; interp. representa o chupacabra que está numa posição y
;; da tela e anda a uma velocidade dy (dy também indica a direção
;; em que ele está apontando)

;exemplos:
(define CC-INICIAL (make-chupacabra X-CC 0 LIMITE-CIMA DY-CC-DEFAULT))
(define CC-MEIO (make-chupacabra X-CC 0 (/ ALTURA 2) DY-CC-DEFAULT))
(define CC-ANTES-VIRAR (make-chupacabra X-CC 0 (+ LIMITE-BAIXO 5) DY-CC-DEFAULT))
(define CC-VIROU (make-chupacabra X-CC 0 LIMITE-BAIXO (- DY-CC-DEFAULT)))
(define CC-CHEGANDO (make-chupacabra X-CC 0 (+ LIMITE-CIMA -5) (- DY-CC-DEFAULT)))
(define CC-VIROU-L-CIMA (make-chupacabra X-CC 0 LIMITE-CIMA DY-CC-DEFAULT))

(define CC-CHAO1 (make-chupacabra (* LARGURA 0.75) -10 Y-CHAO-CC 0))

#;
(define (fn-para-chupacabra cc)
  (... (chupacabra-y cc) (chupacabra-dy cc))
  )

 
; ListaDeChupaCabra é um desses:
;; - empty
;; - (cons ChupaCabra ListaDeChupaCabra)
;; interp. uma lista de chupacabras
(define LDCC1 empty)
(define LDCC2 (cons CC-INICIAL  empty))
(define LDCC3 (cons CC-INICIAL (cons CC-MEIO empty)))

(define LDCC-J1 (list CC-INICIAL
                      (make-chupacabra (/ LARGURA 4) 0 (/ ALTURA 2) DY-CC-DEFAULT)
                      (make-chupacabra (/ LARGURA 4/3) 0 (/ ALTURA 3/2) DY-CC-DEFAULT)))
                      
(define LDCC-CHAO (list CC-CHAO1))

#;
(define (fn-for-ldcc ldcc)
  (cond [(empty? ldcc) (...)]                    ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldcc)                  ;ChupaCabra
                   (fn-for-ldcc (rest ldcc)))])) ;RECURSÃO EM CAUDA



(define-struct vaca (x dx y dy))
;;Vaca é (make-vaca Natural Inteiro)
;;interp. representa a vaca que está numa posição x
;;da tela e anda a uma velocidade dx (dx também indica a direção
;;em que ela está apontando)

;exemplos:
(define VACA-INICIAL (make-vaca LIMITE-ESQUERDO 10 Y 0))
(define VACA-MEIO (make-vaca (/ LARGURA 2) 10 Y 0))
(define VACA-ANTES-VIRAR (make-vaca (+ LIMITE-DIREITO 5) 10 Y 0))
(define VACA-VIRADA (make-vaca LIMITE-DIREITO -10 Y 0))
(define VACA-MEIO-VORTANO (make-vaca (/ LARGURA 2) -10 Y 0))
(define VACA-CHEGANDO (make-vaca 50 -10 Y 0))
(define VACA-ULTRAPASSOU (make-vaca (+ LIMITE-DIREITO 20) 50 Y 0))
(define VACA-NO-LIMITE (make-vaca LIMITE-DIREITO -50 Y 0))

(define VACA-CHAO (make-vaca LIMITE-ESQUERDO 0 Y-CHAO-VACA 0))

#;
(define (fn-para-vaca v)
  (... (vaca-x v) (vaca-dx v))
  )


(define-struct jogo (vaca chupacabras game-over? tspawn))
;; Jogo é (make-jogo Vaca ListaDeChupacabra Boolean)
;; interp. representa um jogo que tem uma vaca
;; e um chupacabra.

(define JOGO-INICIAL (make-jogo VACA-INICIAL
                                (list CC-INICIAL)
                                #false
                                1))
(define JOGO-MEIO (make-jogo VACA-ANTES-VIRAR
                                (list CC-MEIO)
                                #false
                                1))
(define JOGO-ZICA (make-jogo
                   (make-vaca (- (/ LARGURA 2) MEIO-H-CC -5) 10 Y 0)
                   (list CC-MEIO)
                   #false
                   1))
(define JOGO-ZICA-BRABA (make-jogo
                   (make-vaca (- (/ LARGURA 2) MEIO-H-CC -5) 10 Y 0)
                   (list CC-MEIO)
                   #true
                   1))
(define JOGO-ACABOU (make-jogo VACA-MEIO
                               (list CC-MEIO)
                               #true
                               1))

(define JOGO-INICIAL-N-CHUPAS (make-jogo VACA-INICIAL
                                LDCC-J1
                                #false
                                1))

(define JOGO-INICIAL-SPAWNING (make-jogo VACA-INICIAL
                                empty
                                #false
                                0))

(define JOGO-INICIAL-CHAO (make-jogo VACA-CHAO
                                     LDCC-CHAO
                                     #false
                                     1))

#;
(define (fn-para-jogo j)
  (... (jogo-vaca j)
       (jogo-chupacabras j)
       (jogo-game-over? j)))


;; =================
;; Funções:

;; Jogo -> Jogo
;; inicie o mundo com (main JOGO-INICIAL-CHAO)

(define (main j)
  (big-bang j      ; Jogo
            (on-tick proximo-jogo)
            (to-draw desenha-jogo)
            (on-key trata-tecla-press)
            (on-release trata-tecla-solta)))



;; INICIO DA PARTE LÓGICA DO JOGO

;; proximo-jogo : Jogo -> Jogo
;; atualiza o jogo

;(define (proximo-jogo j)  j)

(define (proximo-jogo j)
  (local
    [
     (define spawn? (= (jogo-tspawn j) 0))
     (define pulou-encima-cc? (pulou-encima-algum-chupacabra? (jogo-vaca j) (jogo-chupacabras j)))
     ]
  (cond [(not (false? pulou-encima-cc?))
         (make-jogo (proxima-vaca (quica (jogo-vaca j)))
                    (proximos-chupacabras (remove pulou-encima-cc? (jogo-chupacabras j)))
                    (jogo-game-over? j)
                    (jogo-tspawn j))]
    
        [(colisao-alguma-vaca-chupacabra? (jogo-vaca j) (jogo-chupacabras j))
         (make-jogo (jogo-vaca j)
                    (jogo-chupacabras j)
                    #true
                    (jogo-tspawn j)
                    )]
        

  [else (make-jogo (proxima-vaca (jogo-vaca j)) 
                   (if spawn? 
                       (spawn-chupacabra (proximos-chupacabras (jogo-chupacabras j)))
                       (proximos-chupacabras (jogo-chupacabras j)))                       
                   (jogo-game-over? j)
                   (remainder (+ (jogo-tspawn j) 1) T-SPAWN))]
  )))



;; quica : Vaca -> Vaca
(define (quica v)
  (make-vaca (vaca-x v)
             (vaca-dx v)
             (vaca-y v)
             DY-QUICA))

;; pulou-encima-algum-chupacabra? : Vaca ListaDeChupacabra -> Chupacabra | false
(define (pulou-encima-algum-chupacabra? v ldcc)
  (local [
          (define busca (memf (lambda (cc) (pulou-encima? v cc))
                              ldcc))]
    (if (false? busca)
        #false
        (first busca))))

;; pulou-encima? : Vaca Chupacabra -> Chupacabra | false
(define (pulou-encima? v cc)
  (if (and
       (colisao-vaca-chupacabra? v cc)
       ;(>= (+ (vaca-y v) MEIO-V-VACA) (- (chupacabra-y cc) MEIO-V-CC)))
       (<= (+ (vaca-y v) MEIO-V-VACA) (- (chupacabra-y cc) (/ MEIO-V-CC 8))))
      cc
      #false))
           
                          


;; spawn-chupacabra : ListaDeChupacabra -> ListaDeChupacabra
;; cria novo chupacabra no local especificado

(define (spawn-chupacabra ldcc)
  (cons (make-chupacabra LIMITE-DIREITO -10 Y-CHAO-CC 0) ldcc))


;; proximos-chupacabras : ListaDeChupacabra -> ListaDeChupacabra
;; proximos chupas
(define (proximos-chupacabras ldcc)
  (map proximo-chupacabra ldcc))

(check-expect (proximos-chupacabras
               (list (make-chupacabra X-CC 0 LIMITE-CIMA 10) CC-ANTES-VIRAR))
              (list (make-chupacabra X-CC 0 (+ LIMITE-CIMA 10) 10) CC-VIROU))


;; colisao-alguma-vaca-chupacabra? : Vaca Chupacabras -> Boolean
;; verifica se vaca colidiu com algum chupa

(define (colisao-alguma-vaca-chupacabra? v ldcc)
  (ormap (lambda (cc) (colisao-vaca-chupacabra? v cc)) ldcc))

(check-expect (colisao-alguma-vaca-chupacabra?
               (make-vaca (- (/ LARGURA 2) MEIO-H-CC -5) 10 Y 0)
               LDCC3)
              #true)

(check-expect (colisao-alguma-vaca-chupacabra?
               VACA-INICIAL
               LDCC3)
              #false)


;; colisao-vaca-chupacabra? : Vaca Chupacabra -> Boolean
;; verifica se vaca e chupacabra trombaram
;!!!
;(define (colisao-vaca-chupacabra? v cc) #false)
(define (colisao-vaca-chupacabra? v cc)
  (<= (distancia (vaca-x v) (vaca-y v)
             (chupacabra-x cc) (chupacabra-y cc))
      (+ MEIO-H-VACA MEIO-H-CC)))


(check-expect (colisao-vaca-chupacabra?
               (make-vaca (- (/ LARGURA 2) MEIO-H-CC -5) 10 Y 0)
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
    [(> (chupacabra-x cc) LIMITE-DIREITO)
     (make-chupacabra LIMITE-DIREITO (- (chupacabra-dx cc))
                      (+ (chupacabra-y cc) (chupacabra-dy cc)) (chupacabra-dy cc) )]
    [(< (chupacabra-x cc) LIMITE-ESQUERDO)
     (make-chupacabra LIMITE-ESQUERDO (- (chupacabra-dx cc))
                      (+ (chupacabra-y cc) (chupacabra-dy cc)) (chupacabra-dy cc) )]
    [(> (chupacabra-y cc) LIMITE-BAIXO)
     (make-chupacabra (+ (chupacabra-x cc) (chupacabra-dx cc)) (chupacabra-dx cc)
                      LIMITE-BAIXO (- (chupacabra-dy cc)))]
    [(< (chupacabra-y cc) LIMITE-CIMA)
     (make-chupacabra (+ (chupacabra-x cc) (chupacabra-dx cc)) (chupacabra-dx cc)
                      LIMITE-CIMA (- (chupacabra-dy cc)))]
    [else
     (make-chupacabra (+ (chupacabra-x cc) (chupacabra-dx cc)) (chupacabra-dx cc)
                      (+ (chupacabra-y cc) (chupacabra-dy cc)) (chupacabra-dy cc) )]))



; exemplos / testes
;casos em que ela anda pra direita sem chegar no limite
(check-expect (proximo-chupacabra (make-chupacabra X-CC 0 LIMITE-CIMA 10))
              (make-chupacabra X-CC 0 (+ LIMITE-CIMA 10) 10))
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
(check-expect (proximo-chupacabra (make-chupacabra X-CC 0 (+ LIMITE-CIMA -10) -10))
                            (make-chupacabra X-CC 0 LIMITE-CIMA 10))
(check-expect (proximo-chupacabra (make-chupacabra X-CC 0 (+ LIMITE-CIMA -20) -50))
                            (make-chupacabra X-CC 0 LIMITE-CIMA 50))

;; proxima-vaca : Vaca -> Vaca
;; recebe uma vaca na posicao x e retorna uma vaca com posição
;; x atualizada com o dx
;(define (proxima-vaca v) v)

(define (proxima-vaca v)
  (cond
    [(> (vaca-x v) LIMITE-DIREITO)
     (make-vaca LIMITE-DIREITO (- (vaca-dx v))
                      (+ (vaca-y v) (vaca-dy v)) (vaca-dy v) )]
    [(< (vaca-x v) LIMITE-ESQUERDO)
     (make-vaca LIMITE-ESQUERDO (- (vaca-dx v))
                      (+ (vaca-y v) (vaca-dy v)) (vaca-dy v) )] 
    [else
     (cai v)]))

;; cai: Vaca -> Vaca
(define (cai v)
  (local
    [(define y (+ (vaca-y v) (vaca-dy v)))]
    (make-vaca (+ (vaca-x v) (vaca-dx v))
               (vaca-dx v)
               (if (encima-chao? y)
                   Y-CHAO-VACA
                   y)
               (if (encima-chao? y)
                   0
                   (+ (vaca-dy v) G)))))

;; encima-chao? Numero -> Boolean
(define (encima-chao? y)
  (>= y Y-CHAO-VACA))

              
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
;; desenha chupas
(define (desenha-chupacabras ldcc)
  (foldl overlay CENARIO (map desenha-chupacabra ldcc)))

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
   (vaca-y v)
   CENARIO)               
  )


;; FIM DA PARTE VISUAL

;; INICIO DA LOGICA DE INTERAÇÃO

;; trata-tecla-press : Jogo KeyEvent -> Jogo
;; trata tecla usando trata-tecla-vaca
;!!!
(define (trata-tecla-press j ke)
  (cond
    [(and (jogo-game-over? j) (key=? ke "\r"))
         JOGO-INICIAL-CHAO]
    [else (make-jogo
           (trata-tecla-press-vaca (jogo-vaca j) ke)
           (jogo-chupacabras j)
           (jogo-game-over? j)
           (jogo-tspawn j)
           )]))



(check-expect (trata-tecla-press JOGO-ZICA-BRABA "\r")
              JOGO-INICIAL-CHAO)

;; trata-tecla-solta : Jogo KeyEvent -> Jogo
;; trata tecla usando trata-tecla-vaca
(define (trata-tecla-solta j ke)
  (make-jogo
           (trata-tecla-solta-vaca (jogo-vaca j) ke)
           (jogo-chupacabras j)
           (jogo-game-over? j)
           (jogo-tspawn j)
           ))


;; trata-tecla-press-vaca: Vaca KeyEvent -> Vaca
;; quando tecla espaço é pressionada, vaca deve inverter direção (dx)
;(define (trata-tecla-vaca v ke) v)

(define (trata-tecla-press-vaca v ke)
  (cond [(key=? ke "right")
         (make-vaca (vaca-x v) D-VACA-DEFAULT (vaca-y v) (vaca-dy v))]
        [(key=? ke "left")
         (make-vaca (vaca-x v) (- D-VACA-DEFAULT) (vaca-y v) (vaca-dy v))]       
        [(key=? ke "up")
         (make-vaca (vaca-x v) (vaca-dx v) (vaca-y v) DY-PULO)]
        [else v]))

;; trata-tecla-solta-vaca: Vaca KeyEvent -> Vaca
;; quando tecla espaço é pressionada, vaca deve inverter direção (dx)
;(define (trata-tecla-vaca v ke) v)

(define (trata-tecla-solta-vaca v ke)
   (make-vaca (vaca-x v)
              (cond [(or (key=? ke "right") (key=? ke "left"))
                     0]
                    [else (vaca-dx v)])
              (vaca-y v) (vaca-dy v)))
       

;; FIM DA LOGICA DE INTERAÇÃO



;; CÓDIGO ANTIGO:

;; Vaca -> Vaca
;; inicie o mundo com (main-vaca VACA-INICIAL)
;; 

(define (main-vaca v)
  (big-bang v               ; Vaca   (estado inicial do mundo)
            (on-tick   proxima-vaca)     ; Vaca -> Vaca    
                                 
            (to-draw   desenha-vaca)   ; Vaca -> Image   
                                          
            (on-key    trata-tecla-press-vaca)))    ; Vaca KeyEvent -> Vaca


