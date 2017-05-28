#lang racket
(require srfi/1)

;; Este programa resolve um jogo infinity rotacionando seus blocos
;; até que todos se encaixem entre si. O método de resolução utilizado será
;; por meio de busca com retrocesso (backtracking).
;;
;; ** Conceitos ** 
;; Bloco
;;  Um bloco é uma peça do jogo Infinity. O bloco é representado como um
;;  caractere quando lido ou escrito de um arquivo, e representado como um
;;  número entre 0 e 15 quando processado dentro do programa.
;; Jogo
;;  Um jogo Infinity é um tabuleiro de M linhas e N colunas (tamanho MxN)
;;  preenchidas por blocos. O jogo infinity é dito resolvido quando todos seus
;;  blocos estão conectados. O jogo é representado por uma lista de listas.

;; exporta as funções que podem ser utilizadas em outros arquivos
(provide tamanho
         rotacionar
         encaixa-h?
         encaixa-v?
         seguro?
         resolver
         main)

;; lista com as representações dos blocos como caracteres
(define blocos-reps (string->list " ╹╺┗╻┃┏┣╸┛━┻┓┫┳╋"))

;; Tamanho representa o tamanho de um jogo em altura e largura
;;     altura : Número - quantidade de linhas do jogo.
;;    largura : Número - quantidade de colunas do jogo.
(struct tamanho (altura largura) #:transparent)

;; Bloco -> Bloco
;; --------------
;; Rotaciona um bloco 90 graus em sentido horário
;; Exemplo: (rotacionar 5)
;;          > 10
(define (rotacionar bloco)
  [cond
    ((equal? bloco 0) 0)
    ((equal? bloco 1) 2)
    ((equal? bloco 2) 4)
    ((equal? bloco 3) 6)
    ((equal? bloco 4) 8)
    ((equal? bloco 5) 10)
    ((equal? bloco 6) 12)
    ((equal? bloco 7) 14)
    ((equal? bloco 8) 1)
    ((equal? bloco 9) 3)
    ((equal? bloco 10) 5)
    ((equal? bloco 11) 7)
    ((equal? bloco 12) 9)
    ((equal? bloco 13) 11)
    ((equal? bloco 14) 13)
    ((equal? bloco 15) 15)])


;; Bloco Bloco -> Lógico
;; ---------------------
;; Verifica se o bloco-e se encaixa horizontalmente à esquerda do bloco-d
;; Exemplo: (encaixa-h? 7 9)
;;          > #t
(define (encaixa-h? bloco-e bloco-d)
  [cond
    ;;compara os que NÃO TEM saida a direita com os que tem entrada a esquerda,
    ;;se bater retorna #F já que não forma encaixe, se não tiver entrada na direita, #T
    ((and (or (equal? bloco-e 0) (equal? bloco-e 1) (equal? bloco-e 4)
              (equal? bloco-e 5) (equal? bloco-e 8) (equal? bloco-e 9)
              (equal? bloco-e 12)(equal? bloco-e 13))
          (not(or (equal? bloco-d 8) (equal? bloco-d 9)
                  (equal? bloco-d 10) (equal? bloco-d 11)
                  (equal? bloco-d 12) (equal? bloco-d 13)
                  (equal? bloco-d 14) (equal? bloco-d 15)))) #t)
    ;;compara os que tem saida a direita com os que tem entrada a esquerda, se encaixar retorna #t
    ((and (or (equal? bloco-e 2) (equal? bloco-e 3)
              (equal? bloco-e 6) (equal? bloco-e 7)
              (equal? bloco-e 10)(equal? bloco-e 11)
              (equal? bloco-e 14)(equal? bloco-e 15))
          (or (equal? bloco-d 8) (equal? bloco-d 9)
              (equal? bloco-d 10) (equal? bloco-d 11)
              (equal? bloco-d 12) (equal? bloco-d 13)
              (equal? bloco-d 14) (equal? bloco-d 15))) #t)
    (else
     #f)])


;; Bloco -> Lógico
;; ---------------------
;; Verifica se o bloco se encaixa horizontalmente à esquerda do ultimo bloco (vazio)
;; Exemplo: (encaixa 0)
;;          > #t
(define (encaixa-ultimo-direita? bloco)
  (encaixa-h? bloco 0))

;; Bloco -> Lógico
;; ---------------------
;; Verifica se o bloco se encaixa horizontalmente à direita do primeiro bloco (vazio)
;; Exemplo: (encaixa 0)
;;          > #t
(define (encaixa-ultimo-esquerda? bloco)
  (encaixa-h? 0 bloco))

;; Bloco -> Lógico
;; ---------------------
;; Verifica se o bloco se encaixa verticalmente abaixo do primeiro bloco (vazio)
;; Exemplo: (encaixa 0)
;;          > #t
(define (encaixa-ultimo-acima? bloco)
  (encaixa-v? 0 bloco))

;; Bloco  -> Lógico
;; ---------------------
;; Verifica se o bloco se encaixa verticalmente acima do ultimo bloco (vazio)
;; Exemplo: (encaixa 0)
;;          > #t
(define (encaixa-ultimo-abaixo? bloco)
  (encaixa-v? bloco 0))

;; Lista numero -> Lógico
;; ---------------------
;; Verifica se o bloco e o ultimo da fileira
;; Exemplo: (e-o-ultimo? '(1) 2)
;;          > #t
(define (e-o-ultimo? lista tam)
  (zero? (remainder (add1 (length lista)) tam)))

;; Lista numero -> Lógico
;; ---------------------
;; Verifica se o bloco e o primeiro da fileira
;; Exemplo: (e-o-ultimo? '() 2)
;;          > #t
(define (e-o-primeiro? lista tam)
  (zero? (remainder (length lista) tam)))

;; Bloco Bloco -> Lógico
;; ---------------------
;; Verifica se o bloco-t se encaixa verticalmente acima do bloco-b
;; Exemplo: (encaixa-v? 14 11)
;;          > #t
(define (encaixa-v? bloco-t bloco-b)
  [cond
    ;;compara os que NÃO TEM saida abaixo com os que tem entrada acima,
    ;;se bater retorna #F já que não forma encaixe, se não tiver entrada na abaixo, #T
    ((and (or (equal? bloco-t 0) (equal? bloco-t 1) (equal? bloco-t 2)
              (equal? bloco-t 3) (equal? bloco-t 8) (equal? bloco-t 9)
              (equal? bloco-t 10)(equal? bloco-t 11))
          (not(or (equal? bloco-b 1) (equal? bloco-b 3)
                  (equal? bloco-b 5) (equal? bloco-b 7)
                  (equal? bloco-b 9) (equal? bloco-b 11)
                  (equal? bloco-b 13) (equal? bloco-b 15)))) #t)
    ;;compara os que tem saida a abaixo com os que tem entrada acima, se encaixar retorna #t
    ((and (or (equal? bloco-t 4) (equal? bloco-t 5)
              (equal? bloco-t 6) (equal? bloco-t 7)
              (equal? bloco-t 12)(equal? bloco-t 13)
              (equal? bloco-t 14)(equal? bloco-t 15))
          (or (equal? bloco-b 1) (equal? bloco-b 3)
              (equal? bloco-b 5) (equal? bloco-b 7)
              (equal? bloco-b 9) (equal? bloco-b 11)
              (equal? bloco-b 13) (equal? bloco-b 15))) #t)
    (else
     #f)])

;; Bloco List Tamanho -> Lógico
;; -----------------------------
;; Verifica se um bloco é seguro de ser adicionado a uma solução. Ser 
;; seguro significa que, ao ser adicionado à solução, o bloco se 
;; encaixa a todos os blocos adjacentes à posição em que ele seria 
;; inserido. Uma solução é uma lista de blocos que representa a solução 
;; do jogo até o presente momento. Para facilitar a implementação, 
;; considere que a solução será construída em ordem invertida. Assim, a 
;; solução '(9 7 12 14 6), referente ao um jogo de tamanho 4x3, 
;; representa a seguinte situação:
;; 
;; [6][14][12]    [┏][┳][┓]
;; [7][ 9][ 5] => [┣][┛][┃]
;; [7][  ][  ]    [┣][┳][ ]
;; [ ][  ][  ]    [ ][ ][ ]
;; 
;; A chamada (seguro? 5 '(7 5 9 7 12 14 6) (tamanho 4 3)) deve verificar se 
;; o bloco 5 [┃] é seguro de ser adicionado à solução, isto é, inserido 
;; na posição lin=2, col=3 da situação descrita acima. Observe que para 
;; este exemplo o bloco 5 é seguro, pois ele se encaixa a todos os 
;; blocos adjacentes: ao bloco 9 à esquerda, ao bloco 12 acima e à 
;; borda direita (branco) do tabuleiro. Veja que não houve necessidade 
;; de se verificar o encaixe com o bloco abaixo, já que o mesmo ainda 
;; não existe na solução.
(define (seguro? bloco solucao tam)
  [cond
    ;;se e primeira linha
    ( (if(< (length solucao) (tamanho-largura tam))
         ;;se e primeiro elemento
         (if (e-o-primeiro? solucao (tamanho-largura tam))
             (and (encaixa-ultimo-esquerda? bloco) (encaixa-ultimo-acima? bloco))
             ;;se e ultimo elemento
             (if (e-o-ultimo? solucao (tamanho-largura tam))
                 (and (encaixa-ultimo-direita? bloco) (encaixa-h? (list-ref solucao 0) bloco) (encaixa-ultimo-acima? bloco))
                 ;;se e elemento medio
                 (and (encaixa-ultimo-acima? bloco) (encaixa-h? (list-ref solucao 0) bloco)))) #f))

    ;;se e ultima linha
    ( (if(>= (length solucao) (* (tamanho-altura tam) (sub1 (tamanho-largura tam))))
         ;;se e primeiro elemento
         (if (e-o-primeiro? solucao (tamanho-largura tam))
             (and (encaixa-ultimo-esquerda? bloco) (encaixa-v? (list-ref solucao (sub1 (tamanho-largura tam))) bloco) (encaixa-ultimo-abaixo? bloco))
             ;;se e ultimo elemento
             (if (e-o-ultimo? solucao (tamanho-largura tam))
                 (and (encaixa-ultimo-direita? bloco) (encaixa-h? (list-ref solucao 0) bloco)
                      (encaixa-v? (list-ref solucao (sub1 (tamanho-largura tam))) bloco) (encaixa-ultimo-abaixo? bloco))
                 ;;se e elemento medio
                 (and (encaixa-h? (list-ref solucao 0) bloco)
                      (encaixa-v? (list-ref solucao (sub1 (tamanho-largura tam))) bloco) (encaixa-ultimo-abaixo? bloco)))) #f))
    
    ;;se linha do meio
    ( (if(and (>= (length solucao) (tamanho-largura tam)) (< (length solucao) (* (tamanho-altura tam) (sub1 (tamanho-largura tam)))))
         ;;se e primeiro elemento
         (if (e-o-primeiro? solucao (tamanho-largura tam))
             (and (encaixa-ultimo-esquerda? bloco) (encaixa-v? (list-ref solucao (sub1 (tamanho-largura tam))) bloco))
             ;;se e ultimo elemento
             (if (e-o-ultimo? solucao (tamanho-largura tam))
                 (and (encaixa-ultimo-direita? bloco) (encaixa-h? (list-ref solucao 0) bloco)
                      (encaixa-v? (list-ref solucao (sub1 (tamanho-largura tam))) bloco))
                 ;;se e elemento medio
                 (and (encaixa-h? (list-ref solucao 0) bloco)
                      (encaixa-v? (list-ref solucao (sub1 (tamanho-largura tam))) bloco)))) #f))
        (else
     #f)])
              
;; String -> Jogo
;; Faz a leitura e processa um jogo armazenado em arquivo.
;; Exemplo: (ler-jogo "..\\testes\\casos\\05.txt")
;;          > '((0 6 6 1) (12 15 15 6) (1 10 10 0) (0 2 1 0))
(define (ler-jogo arquivo)
  (map arquive-to-list-number (map list (port->lines (open-input-file arquivo)))))

;; lista -> lista
;;Recebe uma lista e transforma em números do jogo
;; Exemplo: "┏━┳┓"
;;          > '((6 10 14 12))
(define (converte-string-number lista)
  (cond
    [(empty? lista) empty]
    [else
     (cons (list-index (curry equal? (first lista)) blocos-reps) (converte-string-number(rest lista)))]))

;; lista de lista em caracteres -> lista de lista em números
;;Recebe uma lista e transforma em números do jogo
;; Exemplo: ("┏━┳┓" "┣┳┫┃" "┃┣┻┫" "┗┻━┛"
;;          > '((6 10 14 12) (7 14 13 5) (5 7 11 13) (3 11 10 9)))
(define (arquive-to-list-number lst)
  (cond
    [(empty? lst) empty]
    [else
     (append (converte-string-number (string->list (first lst)))
             (arquive-to-list-number (rest lst)))]))

;; Jogo -> void
;; Escreve o jogo na tela codificado em caracteres.
;; Exemplo: (escrever-jogo '((6 10 14 12) (7 14 13 5) (5 7 11 13) (3 11 10 9)))
;;          > ┏━┳┓
;;            ┣┳┫┃
;;            ┃┣┻┫
;;            ┗┻━┛
(define (escrever-jogo jogo) void)
;; Dica: procure pelas funções pré-definidas list->string e string-join

;; Jogo -> Jogo || #f
;; Resolve o jogo Infinity e o retorna resolvido. Caso não seja possível
;; resolvê-lo, retorna o valor falso. Por exemplo, se passado o seguinte jogo:
;;
;; '(( 0  6  6 1)         [ ][┏][┏][╹]
;;   (12 15 15 6)    =>   [┓][╋][╋][┏]
;;   ( 1 10 10 0)         [╹][━][━][ ]
;;   ( 0  2  1 0))        [ ][╺][╹][ ]
;;
;; a função deve retornar:
;;
;; '((0  6 12 4)          [ ][┏][┓][╻]
;;   (6 15 15 9)     =>   [┏][╋][╋][┛]
;;   (1  5  5 0)          [╹][┃][┃][ ]
;;   (0  1  1 0))         [ ][╹][╹][ ]
(define (resolver jogo) #f)

;; List String -> void
;; Esta é a função principal. Esta função é chamada a partir do arquivo
;; infinity-main.rkt
;;
;; args é a lista de parâmetros para o programa.
;;
;; O primeiro e único parâmetro deve ser o nome (caminho) do arquivo que contém 
;; o jogo a ser resolvido. O jogo é representado na forma de caracteres, o qual
;; deverá ser primeiramente convertido para a representação numérica antes de
;; ser resolvido. Veja exemplos de arquivos no diretório de testes.
;;
;; A saída desta função é a escrita na tela do jogo resolvido, representado na
;; forma de caracteres. Caso o jogo não possua solução, nada deve ser escrito na
;; tela.
(define (main args)
  (display args))