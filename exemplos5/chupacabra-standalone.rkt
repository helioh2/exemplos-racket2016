;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname chupacabra-standalone) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct chupacabra (y dy))
(define DY-CC-DEFAULT 20)
(define ALTURA 400)
(define CC-INICIAL (make-chupacabra 0 DY-CC-DEFAULT))
(define CC-MEIO (make-chupacabra (/ ALTURA 2) DY-CC-DEFAULT))
(define CC-ANTES-VIRAR (make-chupacabra (+ ALTURA 5) DY-CC-DEFAULT))
(define CC-VIROU (make-chupacabra ALTURA (- DY-CC-DEFAULT)))
(define CC-CHEGANDO (make-chupacabra -5 (- DY-CC-DEFAULT)))
(define CC-VIROU-L-CIMA (make-chupacabra 0 DY-CC-DEFAULT))


;; proximo-chupacabra : Chupacabra -> Chupacabra
;; recebe um chupacabra na posicao y e retorna um chupacabra com posição
;; y atualizada com o dy
;(define (proximo-chupacabra cc) cc)
(define (proximo-chupacabra cc)
  (cond 
        [(> (chupacabra-y cc) ALTURA)
         (make-chupacabra ALTURA (- (chupacabra-dy cc)))]
        [(< (chupacabra-y cc) 0)
         (make-chupacabra 0 (- (chupacabra-dy cc)))]
        [else
         (make-chupacabra (+ (chupacabra-y cc) (chupacabra-dy cc))
             (chupacabra-dy cc))])
 )


; exemplos / testes
;casos em que ela anda pra direita sem chegar no limite
(check-expect (proximo-chupacabra (make-chupacabra 0 10))
              (make-chupacabra 10 10))
(check-expect (proximo-chupacabra CC-MEIO)
              (make-chupacabra (+ (/ ALTURA 2) DY-CC-DEFAULT)
                         DY-CC-DEFAULT))
; casos em que chega no limite direito e tem que ccirar
(check-expect (proximo-chupacabra CC-ANTES-VIRAR)
              CC-VIROU)

; caso em que ela anda pra esquerda sem chegar no limite 
(check-expect (proximo-chupacabra
               (make-chupacabra (/ ALTURA 2) (- DY-CC-DEFAULT)))
              (make-chupacabra (- (/ ALTURA 2) DY-CC-DEFAULT)
                                       (- DY-CC-DEFAULT)))

; casos em que chega no limite esquerdo e tem que virar
(check-expect (proximo-chupacabra (make-chupacabra -10 -10))
                            (make-chupacabra 0 10))
(check-expect (proximo-chupacabra (make-chupacabra -20 -50))
                            (make-chupacabra 0 50))