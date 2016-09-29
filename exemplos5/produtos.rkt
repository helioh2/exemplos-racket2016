;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname produtos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;;PROBLEMA: vc está fazendo um sistema para gerenciar
;;estoque de um supermercado, e precisa representar e
;;registrar os produtos que estão no estoque. Um produto
;;tem 'nome', 'preço', 'quantidade', 'tipo', 'codigo'.

;;Definição de dados
(define-struct produto (nome preco quantidade tipo codigo))

;;Produto é (make-produto String Numero+ Natural String Natural)
;;interp. são as informações sobre um produto em estoque
;; no supermercado, incluindo nome, preço, qtidade, tipo, codigo
;exemplos
(define ARROZ (make-produto "arroz" 15 100 "comida" 27272727))
(define CERVEJA (make-produto "cerveja" 3.2 50 "bebida" 2827827))
(define AMACIANTE (make-produto "amaciante" 9.3 200 "limpeza" 828282))

#;
(define (fn-para-produto p)
  (... (produto-nome p)
       (produto-preco p)
       (produto-quantidade p)
       (produto-tipo p)
       (produto-codigo p)))



;;PROBLEMA: criar função que recebe um produto e adiciona
;; uma quantidade especificada ao estoque

;; adiciona-estoque: Produto Natural -> Produto
;; aumentar a quantidade do produto no estoque
;(define (adiciona-estoque p n) p)  ;stub


(define (adiciona-estoque p n)
  (make-produto (produto-nome p)
                (produto-preco p)
                (+ (produto-quantidade p) n)
                (produto-tipo p)
                (produto-codigo p)))


;exemplos:
(check-expect (adiciona-estoque ARROZ 50)
              (make-produto (produto-nome ARROZ)
                            (produto-preco ARROZ)
                            (+ (produto-quantidade ARROZ) 50)
                            (produto-tipo ARROZ)
                            (produto-codigo ARROZ))
              )
(check-expect (adiciona-estoque CERVEJA 10)
              (make-produto (produto-nome CERVEJA)
                            (produto-preco CERVEJA)
                            (+ (produto-quantidade CERVEJA) 10)
                            (produto-tipo CERVEJA)
                            (produto-codigo CERVEJA))
              )                            
                            
                            



