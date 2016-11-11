;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname vaca-chupacabras-atirando-angulo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)


;; Programa da vaca na cerca

;;CONSTANTES:
(define LARGURA 600)
(define ALTURA 400)
(define CENARIO (rectangle LARGURA ALTURA "outline" "black"))
(define IMG-VACA-INO (bitmap "vaca-ino.png"))
(define IMG-VACA-VORTANO (flip-horizontal IMG-VACA-INO))
(define IMG-TIRO (ellipse 10 5 "outline" "black"))
(define Y (/ ALTURA 2))

(define IMG-CC (scale 0.5 (bitmap "chupacabra.jpg")))

(define X-CC (/ LARGURA 2))
(define DY-CC-DEFAULT 5)

(define MEIO-H-VACA (/ (image-width IMG-VACA-INO) 2 ))
(define MEIO-V-VACA (/ (image-height IMG-VACA-INO) 2 ))
(define MEIO-H-CC (/ (image-width IMG-CC) 2 ))
(define MEIO-H-TIRO (/ (image-width IMG-TIRO) 2 ))

(define LIMITE-DIREITO (- LARGURA MEIO-H-VACA))
(define LIMITE-ESQUERDO MEIO-H-VACA)
(define LIMITE-BAIXO (- ALTURA MEIO-V-VACA))
(define LIMITE-CIMA MEIO-V-VACA)

(define TELA-GAME-OVER (overlay (text "GAME OVER" 30 "red") CENARIO))

(define X-SPAWN (* LARGURA 0.75))
(define Y-SPAWN (* ALTURA 0.25))
(define T-SPAWN (* 20 2))

(define D-VACA-DEFAULT 10)
(define D-TIRO 30)

(define IMG-FLECHA (rotate 270 (above
                    (triangle 10  "solid" "blue")
                    (rectangle 5 MEIO-H-VACA "solid" "blue")
                    (rectangle 5 (* MEIO-H-VACA 3) "solid" "transparent")
                    
                    )))

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
                      

#;
(define (fn-for-ldcc ldcc)
  (cond [(empty? ldcc) (...)]                    ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldcc)                  ;ChupaCabra
                   (fn-for-ldcc (rest ldcc)))])) ;RECURSÃO EM CAUDA



(define-struct vaca (x dx y dy angulo))
;;Vaca é (make-vaca Natural Inteiro)
;;interp. representa a vaca que está numa posição x
;;da tela e anda a uma velocidade dx (dx também indica a direção
;;em que ela está apontando)

;exemplos:
(define VACA-INICIAL (make-vaca LIMITE-ESQUERDO 10 Y 0 0))
(define VACA-MEIO (make-vaca (/ LARGURA 2) 10 Y 0 0))
(define VACA-ANTES-VIRAR (make-vaca (+ LIMITE-DIREITO 5) 10 Y 0 0))
(define VACA-VIRADA (make-vaca LIMITE-DIREITO -10 Y 0 0))
(define VACA-MEIO-VORTANO (make-vaca (/ LARGURA 2) -10 Y 0 0))
(define VACA-CHEGANDO (make-vaca 50 -10 Y 0 0))
(define VACA-ULTRAPASSOU (make-vaca (+ LIMITE-DIREITO 20) 50 Y 0 0))
(define VACA-NO-LIMITE (make-vaca LIMITE-DIREITO -50 Y 0 0))
(define VACA-PARADA (make-vaca LIMITE-ESQUERDO 0 Y 0 0))

#;
(define (fn-para-vaca v)
  (... (vaca-x v) (vaca-dx v))
  )

(define-struct tiro (x dx y dy))
;; Tiro é (make-tiro Natural Inteiro Natural)
;; interp. um tiro que apenas na horizontal
(define TIRO-PADRAO (make-tiro 100 30 100 0))
#;
(define (fn-para-tiro t)
  (... (tiro-x t) (tiro-dx t))
  )


(define-struct jogo (vaca chupacabras game-over? tspawn tiros))
;; Jogo é (make-jogo Vaca ListaDeChupacabra Boolean)
;; interp. representa um jogo que tem uma vaca
;; e um chupacabra.

(define JOGO-INICIAL (make-jogo VACA-INICIAL
                                (list CC-INICIAL)
                                #false
                                1
                                empty))
(define JOGO-MEIO (make-jogo VACA-ANTES-VIRAR
                                (list CC-MEIO)
                                #false
                                1
                                empty))
(define JOGO-ZICA (make-jogo
                   (make-vaca (- (/ LARGURA 2) MEIO-H-CC -5) 10 Y 0 0)
                   (list CC-MEIO)
                   #false
                   1
                   empty))
(define JOGO-ZICA-BRABA (make-jogo
                   (make-vaca (- (/ LARGURA 2) MEIO-H-CC -5) 10 Y 0 0)
                   (list CC-MEIO)
                   #true
                   1
                   empty))
(define JOGO-ACABOU (make-jogo VACA-MEIO
                               (list CC-MEIO)
                               #true
                               1
                               empty))

(define JOGO-INICIAL-N-CHUPAS (make-jogo VACA-INICIAL
                                LDCC-J1
                                #false
                                1
                                empty))

(define JOGO-INICIAL-SPAWNING (make-jogo VACA-PARADA
                                empty
                                #false
                                0
                                empty))

#;
(define (fn-para-jogo j)
  (... (jogo-vaca j)
       (jogo-chupacabras j)
       (jogo-game-over? j)))


;; =================
;; Funções:

;; Jogo -> Jogo
;; inicie o mundo com (main JOGO-INICIAL-SPAWNING)

(define (main j)
  (big-bang j      ; Jogo
            (on-tick proximo-jogo)
            (to-draw desenha-jogo)
            (on-key trata-tecla)
            (on-release trata-tecla-release)
            (on-mouse trata-mouse)))



;; INICIO DA PARTE LÓGICA DO JOGO

;; proximo-jogo : Jogo -> Jogo
;; atualiza o jogo

;(define (proximo-jogo j)  j)

(define (proximo-jogo j)
  (local
    [
     (define spawn? (= (jogo-tspawn j) 0))
     (define acertou-chupacabra? (colisao-tiros-chupacabras? (jogo-tiros j) (jogo-chupacabras j)))
     ]
  (cond [(colisao-alguma-vaca-chupacabra? (jogo-vaca j) (jogo-chupacabras j))
         (make-jogo (jogo-vaca j)
                    (jogo-chupacabras j)
                    #true
                    (jogo-tspawn j)
                    (jogo-tiros j)
                   )]

  [else (make-jogo (proxima-vaca (jogo-vaca j))
                   (cond
                     [spawn? 
                          (spawn-chupacabra (proximos-chupacabras (jogo-chupacabras j)))]
                     [(list? acertou-chupacabra?)
                          (proximos-chupacabras
                           (remove (second acertou-chupacabra?) (jogo-chupacabras j)))]                         
                     [else
                      (proximos-chupacabras (jogo-chupacabras j))])                       
                   (jogo-game-over? j)
                   (remainder (+ (jogo-tspawn j) 1) T-SPAWN)
                   (if (list? acertou-chupacabra?)
                       (proximos-tiros
                        (remove (first acertou-chupacabra?) (jogo-tiros j)))
                       (proximos-tiros (jogo-tiros j))))]
  )))

;caso normal
(check-expect (proximo-jogo JOGO-INICIAL)
              (make-jogo (make-vaca (+ LIMITE-ESQUERDO 10) 10 Y 0 0)
                         (list (make-chupacabra X-CC 0 (+ LIMITE-CIMA DY-CC-DEFAULT) DY-CC-DEFAULT))
                         #false
                         2
                         empty))

(check-expect (proximo-jogo JOGO-ZICA)
              JOGO-ZICA-BRABA)


;; colisao-tiros-chupacabras? : ListaDeTiro ListaDeChupacabras -> (pair Tiro Chupacabra) | false
;; verifica se tiros acertaram chupacabras

(define (colisao-tiros-chupacabras? ldt ldcc)
  (local
    [
     (define (colisao-tiro-chupacabra? t cc)
       (if (<= (distancia (tiro-x t) (tiro-y t)
                          (chupacabra-x cc) (chupacabra-y cc))
               (+ MEIO-H-TIRO MEIO-H-CC))
           (list t cc)
           #false))

     (define (cria-pares item lista)
       (map (lambda (item2) (list item item2)) lista))

     (define (produto-cartesiano list1 list2)
       (cond [(empty? list1) empty]
             [else
              (append (cria-pares (first list1) list2)
                      (produto-cartesiano (rest list1) list2))]))

     (define busca
       (memf (lambda (par) (colisao-tiro-chupacabra? (first par) (second par)))
           (produto-cartesiano ldt ldcc)))
     
     ]
    (if (false? busca)
        #false
        (first busca))))


;; proximos-tiros : Tiros -> Tiros
(define (proximos-tiros ldt)
  (filter (lambda (t) (and (>= (tiro-x t) 0) (<= (tiro-x t) LARGURA)))
          (map proximo-tiro ldt)))

;; proximo-tiro : Tiro -> Tiro
(define (proximo-tiro t)
  (make-tiro (+ (tiro-x t) (tiro-dx t)) (tiro-dx t) (+ (tiro-y t) (tiro-dy t)) (tiro-dy t))) 

;; spawn-chupacabra : ListaDeChupacabra -> ListaDeChupacabra
;; cria novo chupacabra no local especificado

(define (spawn-chupacabra ldcc)
  (cons (make-chupacabra X-SPAWN (random 5) Y-SPAWN (random 5)) ldcc))


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
               (make-vaca (- (/ LARGURA 2) MEIO-H-CC -5) 10 Y 0 0)
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
               (make-vaca (- (/ LARGURA 2) MEIO-H-CC -5) 10 Y 0 0)
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
                      (+ (vaca-y v) (vaca-dy v)) (vaca-dy v) (vaca-angulo v) )]
    [(< (vaca-x v) LIMITE-ESQUERDO)
     (make-vaca LIMITE-ESQUERDO (- (vaca-dx v))
                      (+ (vaca-y v) (vaca-dy v)) (vaca-dy v) (vaca-angulo v) )]
    [(> (vaca-y v) LIMITE-BAIXO)
     (make-vaca (+ (vaca-x v) (vaca-dx v)) (vaca-dx v)
                      LIMITE-BAIXO (- (vaca-dy v)) (vaca-angulo v))]
    [(< (vaca-y v) LIMITE-CIMA)
     (make-vaca (+ (vaca-x v) (vaca-dx v)) (vaca-dx v)
                      LIMITE-CIMA (- (vaca-dy v)) (vaca-angulo v))]
    [else
     (make-vaca (+ (vaca-x v) (vaca-dx v)) (vaca-dx v)
                      (+ (vaca-y v) (vaca-dy v)) (vaca-dy v) (vaca-angulo v) )]))




; exemplos / testes
;casos em que ela anda pra direita sem chegar no limite
(check-expect (proxima-vaca (make-vaca LIMITE-ESQUERDO 10 Y 0 0))
              (make-vaca (+ LIMITE-ESQUERDO 10) 10 Y 0 0))
(check-expect (proxima-vaca VACA-MEIO)
              (make-vaca (+ (/ LARGURA 2) 10)
                         10
                         Y 0 0))
; casos em que chega no limite direito e tem que virar
(check-expect (proxima-vaca VACA-ANTES-VIRAR)
              VACA-VIRADA)
(check-expect (proxima-vaca VACA-ULTRAPASSOU)
                            VACA-NO-LIMITE)
; caso em que ela anda pra esquerda sem chegar no limite 
(check-expect (proxima-vaca VACA-MEIO-VORTANO)
                            (make-vaca (- (/ LARGURA 2) 10)
                                       -10
                                       Y 0 0))

; casos em que chega no limite esquerdo e tem que virar
(check-expect (proxima-vaca (make-vaca (+ LIMITE-ESQUERDO -10) -10 Y 0 0))
                            (make-vaca LIMITE-ESQUERDO 10 Y 0 0))
(check-expect (proxima-vaca (make-vaca (+ LIMITE-ESQUERDO -20) -50 Y 0 0))
                            (make-vaca LIMITE-ESQUERDO 50 Y 0 0))

              
;; FIM DA PARTE LÓGICA


;; INICIO DA PARTE VISUAL

;; desenha-jogo : Jogo -> Image
;; desenha o jogo
;!!!
(define (desenha-jogo j)
  (if (jogo-game-over? j) TELA-GAME-OVER
  (overlay
   (desenha-tiros (jogo-tiros j))
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


(define (desenha-tiros ldt)
  (foldl overlay CENARIO (map desenha-tiro ldt)))

(define (desenha-tiro t)
  (place-image IMG-TIRO (tiro-x t) (tiro-y t) CENARIO))


;; desenha-vaca: Vaca -> Image
;; retorna a representação do cenário com a vaca
#;
(define (fn-para-vaca v)
  (... (vaca-x v) (vaca-dx v))
  )

(define (desenha-vaca v)

  (place-image
   (overlay (rotate (vaca-angulo v) IMG-FLECHA)
            (if (< (vaca-dx v) 0)
                IMG-VACA-VORTANO
                IMG-VACA-INO))
   (vaca-x v)
   (vaca-y v)
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
         JOGO-INICIAL-SPAWNING]
    [(key=? ke " ")
     (make-jogo
           (trata-tecla-vaca (jogo-vaca j) ke)
           (jogo-chupacabras j)
           (jogo-game-over? j)
           (jogo-tspawn j)
           (cons (make-tiro (vaca-x (jogo-vaca j))
                            D-TIRO
                            (vaca-y (jogo-vaca j))
                            0)
                 (jogo-tiros j))
           )]
    [else (make-jogo
           (trata-tecla-vaca (jogo-vaca j) ke)
           (jogo-chupacabras j)
           (jogo-game-over? j)
           (jogo-tspawn j)
           (jogo-tiros j)
           )]))



(check-expect (trata-tecla JOGO-ZICA-BRABA "\r")
              JOGO-INICIAL-SPAWNING)

;; trata-tecla-vaca: Vaca KeyEvent -> Vaca
;; quando tecla espaço é pressionada, vaca deve inverter direção (dx)
;(define (trata-tecla-vaca v ke) v)

(define (trata-tecla-vaca v ke)
  (cond [(key=? ke "right")
         (make-vaca (vaca-x v) D-VACA-DEFAULT (vaca-y v) 0 (vaca-angulo v))]
        [(key=? ke "left")
         (make-vaca (vaca-x v) (- D-VACA-DEFAULT) (vaca-y v) 0 (vaca-angulo v))]
        [(key=? ke "down")
         (make-vaca (vaca-x v) 0 (vaca-y v) D-VACA-DEFAULT (vaca-angulo v))]
        [(key=? ke "up")
         (make-vaca (vaca-x v) 0 (vaca-y v) (- D-VACA-DEFAULT) (vaca-angulo v))]
        [else v]))


(define (trata-tecla-release j ke)
  (make-jogo
    (trata-tecla-vaca-release (jogo-vaca j) ke)
    (jogo-chupacabras j)
    (jogo-game-over? j)    
    (jogo-tspawn j)
    (jogo-tiros j)
    ))

(define (trata-tecla-vaca-release v ke)
  (if (member ke (list "right" "left" "up" "down"))
      (make-vaca (vaca-x v) 0 (vaca-y v) 0 (vaca-angulo v))
      v)) 

;; trata-mouse : Jogo Integer Integer MouseEvent -> Jogo
(define (trata-mouse j x y me)
  (make-jogo
    (if (mouse=? me "move")
        (muda-angulo (jogo-vaca j) x y)
        (jogo-vaca j))
    (jogo-chupacabras j)
    (jogo-game-over? j)    
    (jogo-tspawn j)
     (if (mouse=? me "button-down")
        (atira (jogo-vaca j) (jogo-tiros j))
        (jogo-tiros j))
    ))

; Vaca Numero Numero -> Vaca
(define (muda-angulo v x y)
  (make-vaca
   (vaca-x v) (vaca-dx v) (vaca-y v) (vaca-dy v) 
   (calcula-angulo (vaca-x v) (inverte-y (vaca-y v))
                   x (inverte-y y))))

(require racket/math)  ;para poder usar funcoes trigonometricas

;; Numero Numero Numero Numero -> Numero
; Calcula angulo entre 2 pontos
(define (calcula-angulo x1 y1 x2 y2)
  (radians->degrees (atan (- y2 y1) (- x2 x1))))

(define (inverte-y y)
  (- ALTURA y))  ;INVERTER Y PARA OS CALCULOS FICAREM DE ACORDO COM O PRIMEIRO QUADRANTE DO PLANO CARTESIANO
  

;; lanca-bola Numero Numero : Bola -> Bola
(define (atira v ldt)
  (local
    [
     (define angulo (degrees->radians (vaca-angulo v)))
     ]
  (cons (make-tiro
         (vaca-x v)
         (* 10 (cos angulo))
         (vaca-y v)
         (* 10  (- (sin angulo))))
        ldt)))


;; FIM DA LOGICA DE INTERAÇÃO




