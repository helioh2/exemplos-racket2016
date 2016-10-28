;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname listas) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))


;; ListaDeString é um desses:
;; - empty
;; - (cons String ListaDeString)

(define L0 empty)
(define L1 (list "ARROZ"))
(define L3 (list "BIFE" "FEIJAO" "ARROZ"))

#;
(define (fn-para-lds lds)
  (cond [(empty? lds) (...)]                   ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first lds)                 ;String
                   (fn-for-lds (rest lds)))])) ;RECURSÃO EM CAUDA


;; comprar-tudo: ListaDeString -> ListaDeString
;; colocar um "comprei" na frente de todos os itens da lista
;(define (comprar-tudo lds) lds)

(define (comprar-tudo lds)
  (cond [(empty? lds) empty]                 
        [else (cons (string-append "comprei " (first lds))          
                   (comprar-tudo (rest lds)))])) ;RECURSÃO EM CAUDA

(check-expect (comprar-tudo empty) empty)
(check-expect (comprar-tudo L3)
              (list "comprei BIFE" "comprei FEIJAO" "comprei ARROZ"))


;; somatorio : (lista de Numero) -> Numero

(define (somatorio ldn)
  (cond [(empty? ldn) 0]                   
        [else (+ (first ldn)                 
                   (somatorio (rest ldn)))])) 

(check-expect (somatorio empty) 0)
(check-expect (somatorio (list 1 2 3)) 6)


;; multiplica-dois : (lista de Numero) -> (lista de Numero)

(define (multiplica-dois ldn)
  (cond [(empty? ldn) empty]                   ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (cons (* 2 (first ldn))                 ;String
                   (multiplica-dois (rest ldn)))])) ;RECURSÃO EM CAUDA


(check-expect (multiplica-dois empty) empty)
(check-expect (multiplica-dois (list 1 2 3)) (list 2 4 6))


(define (dobro n)
  (* n 2))
  
(define (multiplica-dois-v2 ldn)
  (map dobro ldn))

(define (somatorio-v2 ldn)
  (foldl + 0 ldn))


(define (nosso-remove n ldn)
  (cond [(empty? ldn) empty]                   
        [else (if (not (= (first ldn) n))
                  (cons (first ldn) (remove n (rest ldn)))
                  (remove n (rest ldn)))]
        )) 