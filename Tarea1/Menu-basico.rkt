#lang racket


; Juego Dara
; El juego consiste en formar tres fichas en una línea y eliminar la mayor cantidad de fichas
; del oponente hasta que este ya no pueda formar tres fichas en línea.
;
; Instrucciones de ejecución
; a) Abrir DrRacket.
; b) Open.
; c) Seleccionamos la ubicación de este archivo.
; d) Seleccionamos Run.
; Con esto el programa será ejecutado y le pedirá una opción para empezar a jugar
; Introducimos 1 para iniciar el juego con rellenado de tablero manual
; Introducimos 2 para iniciar el juego con rellenado de tablero aleatorio
; Introducimos 3 Para salir

; Made by Ricardo Soto && Brandon Redondo

(struct player (symbol moves moveQuant) #:mutable)

; Función que crea una matriz de 6x5.
(define matriz (build-list 6 (lambda (i) (build-list 5 (lambda (j) '#\ )))))
(define hashGen (hash))


; Función que despligue el menú principal.
(define (display-menu)
  (displayln "Menu:")
  (displayln "1. Llenar manualmente")
  (displayln "2. Llenar aleatoriamente")  
  (displayln "3. Salir"))

; Función que recibe una opción por teclado
; Entrada: Int
; Salida: Opción escogida.
(define (get-user-choice)
  (display "Ingresa una opción ")
  (flush-output)
  (define input (read-line))
  (cond
    [(string=? input "1") 'option1]
    [(string=? input "2") 'option2]
    [(string=? input "3") 'exit]

    [else
     (displayln "Opción inválida, inténtalo de nuevo")
     (get-user-choice)]))

; Función que despliega la matriz en pantalla
; Entrada: La matriz, int fila, int columna.
; Salida: La matriz con ese valor en pantalla.
(define (displayMat matrix i j)
  (if (>= i (length matrix))
      (display #\newline)

      (if (< j (length (list-ref matrix i)))
          (begin
            (display (list-ref (list-ref matrix i) j))
            (display #\ )
            (display #\ )
            (display #\|)
            (display #\ )
            (display #\ )
            (displayMat matrix i (+ j 1)))

          (begin
            (display i)
            (display #\newline)
            (display #\|)
            (displayMat matrix (+ i 1) 0)))))

(define (getHash matrix i j hash suma)
  (define simbolo 0)
  (if (>= i (length matrix))
      hash

      (if (< j (length (list-ref matrix i)))
          (begin
                                                       
            (set! simbolo(list-ref (list-ref matrix i) j))
            (if (equal? simbolo "O")
                (set! hash(+ hash (* suma 2)))
                (if(equal? simbolo "X")
                   (set! hash(+ hash (* suma 1)))
                   (set! hash hash)
                   )
                
                )
            
            (getHash matrix i (+ j 1) hash (+ suma 1))
            
            )
          (begin
            
            
            
            (getHash matrix (+ i 1) 0 hash (+ suma 1)))
          )))
 
; Función que hace el movimiento de pieza.
; Entrada: La matriz y el jugador actual
; Salida: Recibe la ficha a mover por teclado y realiza el cambio de estado.
(define (movePieces matriz currPlayer)
  (display "Ingresa la ficha, ejemplo : 1 2  ")
  (flush-output)
  (define input (read-line))
  (define fila (- (char->integer (string-ref input 0)) 48))
  (define col (- (char->integer (string-ref input 2)) 48))
  (define volcado (list-set (list-ref matriz fila) col (player-symbol currPlayer)))
  
  (list-set matriz fila volcado))

; Función que cuenta la cantidad de simbolos en la matriz
; Entrada: 0, 0, contador, Carácter: "X" o "O"
; Salida: Cantidad de cáracteres en la matriz
(define (countSimbolo i j count simbolo)
  (if (>= i (length matriz))
      (<= count 2)
      (if (< j (length (list-ref matriz i)))
          (begin
            (if (equal? (list-ref (list-ref matriz i) j) simbolo)
                (countSimbolo i (+ j 1) (+ count 1) simbolo)
                (countSimbolo i (+ j 1) count simbolo)))
          (begin
            (countSimbolo (+ i 1) 0 count simbolo)))))

(define (countSimbolo2 matrix i j count simbolo)
  (if (>= i (length matrix))
      count
      (if (< j (length (list-ref matrix i)))
          (begin
            (if (equal? (list-ref (list-ref matrix i) j) simbolo)
                (countSimbolo2 matrix i (+ j 1) (+ count 1) simbolo)
                (countSimbolo2 matrix i (+ j 1) count simbolo)))
          (begin
            (countSimbolo2 matrix (+ i 1) 0 count simbolo)))))

; Función que cuenta los espacios que han sido llenados
; Entrada: 0, 0, cantidad de espacios libres
; Salida: Elemento entero
(define (countFilledSpaces i j count)
  (if (>= i (length matriz))
      (>= count 24)
      (if (< j (length (list-ref matriz i)))
          (begin
            (if (not (equal? (list-ref (list-ref matriz i) j) '#\ ))
                (countFilledSpaces i (+ j 1) (+ count 1))
                (countFilledSpaces i (+ j 1) count)))

          (begin

            (countFilledSpaces (+ i 1) 0 count)))))

; Función que elimina una ficha del jugador contrario solicitando cual captura mediante input
; Entrada: Jugador Actual
; Salida: Elemento removido de la matriz
(define (cogerFichaEnemiga currPlayer)
  (displayln #\ )

  (displayln
   "¡Lograste alinear 3 elementos, coje una ficha enemiga! Ingresa la ficha, ejemplo: bash/user$0 1")

  (flush-output)
  (define input (read-line))
  (define fila (- (char->integer (string-ref input 0)) 48))
  (define col (- (char->integer (string-ref input 2)) 48))
  (define jugador (player-symbol currPlayer))
  (define simbolocontrario "")
  (if (equal? jugador "O") (set! simbolocontrario "X") (set! simbolocontrario "O"))

  ; validar que la ficha sea la contraria a currPlayer
  (displayln simbolocontrario)
  (define volcado (list-set (list-ref matriz fila) col #\ ))
  (if (equal? (list-ref (list-ref matriz fila) col) simbolocontrario)
      (if (formaLinea matriz fila col simbolocontrario)
          (begin
            (displayln "¡Eror, no se puede quitar esa ficha, intenta con otra")
            (cogerFichaEnemiga currPlayer))

          (set! matriz (list-set matriz fila volcado)))

      (begin
        (displayln "¡Eror, no se puede quitar esa ficha, intenta con otra")
        (cogerFichaEnemiga currPlayer))))
(define (displayPlayMenu player1 player2 currPlayer)

  (displayln (string-append "Turno de: " (player-symbol currPlayer)))
  (define cnd (moveValidPieces currPlayer))

  (if (equal? cnd #t)
      (if (equal? currPlayer player1) (set! currPlayer player2) (set! currPlayer player1))
      (set! currPlayer currPlayer))

  (display " 0     1     2     3     4   \n")
  (display #\|)
  (displayMat matriz 0 0)
  (if (countSimbolo 0 0 0 "O")
      (displayln "El ganador es X")
      (if (countSimbolo 0 0 0 "X")
          (displayln "El ganador es O")
          (displayPlayMenu player1 player2 currPlayer))))

; Función que determina si un movimiento es válido.
; Entrada: Fila Inicio, Columna Inicio, Fila Despues, Columna despues, Jugador actual
; Salida: True/False
(define (moveIsValid rowIn colIn rowDes colDes currPlayer)
  (define cond1 (equal? (player-symbol currPlayer) (list-ref (list-ref matriz rowIn) colIn)))
  (define cond2 (equal? (list-ref (list-ref matriz rowDes) colDes) #\ ))
  (define rowMove (and (= (abs (- rowIn rowDes)) 1) (= colIn colDes)))
  (define colMove (and (= (abs (- colIn colDes)) 1) (= rowIn rowDes)))

  (and cond1 cond2 (or rowMove colMove)))



(define (moveV filaIn colIn filaDes colDes currPlayer mat)
  
  (define cond1 #f)
  (define cond2 #f)
  (define fueraRango (or (< filaIn 0) (< filaDes 0) (< colIn 0) (< colDes 0)))
  (if (or (>= filaIn 6) (>= filaDes 6) (>= colDes 5) (>= colIn 5) fueraRango)
      #f
      (begin
        
        (set! cond1 (equal? currPlayer (list-ref (list-ref mat filaIn) colIn)))
            (set! cond2 (equal? (list-ref (list-ref mat filaDes) colDes) #\ ))
      (if (and cond1 cond2)
          
            
            #t
            
            
          #f
          ))
  )
  )
  
;Permite Validar si el usuario está realizando el movimiento correctamente, esta vez para llamar a realizar a jugada
(define (keepValidating currPlayer input)
  (define filaIn (- (char->integer (string-ref input 0)) 48))
  (define colIn (- (char->integer (string-ref input 2)) 48))
  (define filaDes (- (char->integer (string-ref input 7)) 48))
  (define colDes (- (char->integer (string-ref input 9)) 48))

  (define fueraRango (or (< filaIn 0) (< filaDes 0) (< colIn 0) (< colDes 0)))
  
  (if (or (>= filaIn 6) (>= filaDes 6) (>= colDes 5) (>= colIn 5) fueraRango)
      (begin
        (displayln
         "Movimiento fuera de rango, ingresa el movimiento correctamente, ejemplo: bash/user$1 2 -> 3 2 ")
        #f)
      (movePiecesGame currPlayer input)))
;Permite Validar si el usuario está realizando el movimiento correctamente
(define (moveValidPieces currPlayer)

  (flush-output)
  (define input (read-line))
  (if (= (string-length input) 10)
      (keepValidating currPlayer input)
      (begin
        (displayln "Error de sintaxis, ingresa bien el movimiento, ejemplo: bash/user$1 2 -> 3 2 ")
        #f)))

(define (matrix-set matrix i j valor)
  (define volcado (list-set(list-ref matrix i) j valor))
  (list-set matrix i volcado)
  
  
  )
; Función que realiza el movimiento una vez que se colocadas todas las fichas.
; Entrada: Jugador actual
; Salida: Movimiento de la ficha si es válido
(define (movePiecesGame currPlayer input)

  (displayln "Ingresa el movimiento, debe respetar los espacios, ejemplo: bash/user$1 2 -> 3 2 ")

  (define filaIn (- (char->integer (string-ref input 0)) 48))
  (define colIn (- (char->integer (string-ref input 2)) 48))
  (define filaDes (- (char->integer (string-ref input 7)) 48))
  (define colDes (- (char->integer (string-ref input 9)) 48))

  (define simbolo (player-symbol currPlayer))

  (define volcado
    (list-set (list-ref matriz filaDes) colDes (list-ref (list-ref matriz filaIn) colIn)))
  (define cond (moveIsValid filaIn colIn filaDes colDes currPlayer))
  (if cond

      (set! matriz (list-set matriz filaDes volcado))

      #\
)
  (define anterior (list-set (list-ref matriz filaIn) colIn #\ ))

  ; Aqui se debe validar el movimiento (que no hayan fichas donde lo mueve, que haga un 3 en línea
  
  
  (if cond
      (begin
        
        (set! matriz (list-set matriz filaIn anterior))
        
        (set! hashGen (hash))
        
        
        (if (formaLinea matriz filaDes colDes simbolo)
            (begin
              (display " 0     1     2     3     4   \n")
              (display #\|)
              (displayMat matriz 0 0)
              (cogerFichaEnemiga currPlayer)
              #t)
            #t)
        #t)

      (begin
        (displayln #\ )

        (displayln "Movimiento inválido XXX")

        (displayln #\ )
        #f)))

; Función que determinar el turno de cada jugador.
; Entrada: Jugador1, Jugador2 y el jugador actual.
; Salida: Indicación del jugador actual
(define (playerTurnMenu player1 player2 currPlayer)

  (if (equal? currPlayer player1) (set! currPlayer player2) (set! currPlayer player1))
  (displayln (string-append "Turno de: " (player-symbol currPlayer)))

  (set! matriz (movePieces matriz currPlayer))

  (display " 0     1     2     3     4   \n")
  (display #\|)
  (displayMat matriz 0 0)

  (if (countFilledSpaces 0 0 0)
      (begin
        (writeln "Inicio del juego!")
        (displayPlayMenu currPlayer player1 player2))
      (playerTurnMenu player1 player2 currPlayer)))

; Función que verifica si se formó una linea consecutiva de tres carácteres iguales
; Entrada: Fila, Columna, Caracter
; Salida: True/False
(define (formaLinea matriz fila col caracter)
  (define ancho (length (first matriz))) ; ancho de la matriz
  (define alto (length matriz)) ; alto de la matriz
  (define (enRango f c) ; verifica que una posición está dentro del rango de la matriz
    (and (>= f 0) (< f alto) (>= c 0) (< c ancho)))

  (define (verificarArriba f c n)
    (cond
      [(= n 2) #t] ; si ya se encontraron tres caracteres consecutivos, se ha formado la línea
      [(not (enRango f c)) #f] ; si estamos fuera de la matriz, la línea no se ha formado
      [(not (eq? (list-ref (list-ref matriz f) c) caracter))
       #f] ; si el caracter en esta posición no es el buscado, la línea no se ha formado
      [else
       (verificarArriba
        (- f 1)
        c
        (+ n 1))])) ; si seguimos buscando arriba, incrementamos la cuenta de caracteres consecutivos

  (define (verificarAbajo f c n)
    (cond
      [(= n 2) #t]
      [(not (enRango f c)) #f]
      [(not (eq? (list-ref (list-ref matriz f) c) caracter)) #f]
      [else (verificarAbajo (+ f 1) c (+ n 1))]))

  (define (verificarIzquierda f c n)
    (cond
      [(= n 2) #t]
      [(not (enRango f c)) #f]
      [(not (eq? (list-ref (list-ref matriz f) c) caracter)) #f]
      [else (verificarIzquierda f (- c 1) (+ n 1))]))

  (define (verificarDerecha f c n)
    (cond
      [(= n 2) #t]
      [(not (enRango f c)) #f]
      [(not (eq? (list-ref (list-ref matriz f) c) caracter)) #f]
      [else (verificarDerecha f (+ c 1) (+ n 1))]))
  (define (verificarMedioVert f c n)
    (cond
      [(= n 2) #t]
      [(not (enRango f c)) #f]
      [(not (eq? (list-ref (list-ref matriz f) c) caracter)) #f]
      [else (verificarMedioVert (+ f 2) c (+ n 1))]))
  (define (verificarMedioHor f c n)
    (cond
      [(= n 2) #t]
      [(not (enRango f c)) #f]
      [(not (eq? (list-ref (list-ref matriz f) c) caracter)) #f]
      [else (verificarMedioVert f (+ c 2) (+ n 1))]))
  (or (eq? (list-ref (list-ref matriz fila) col) 0)
      #f
      (verificarArriba (- fila 1) col 0) ; se forma línea hacia arriba
      (verificarAbajo (+ fila 1) col 0) ; se forma línea hacia abajo
      (verificarIzquierda fila (- col 1) 0) ; se forma línea hacia la izquierda
      (verificarMedioVert (- fila 1) col 0) ; se forma línea vertical en el medio
      (verificarMedioHor fila (- col 1) 0) ; se forma línea horizontal en el medio
      (verificarDerecha fila (+ col 1) 0))) ; se forma línea hacia la derecha

; Función que genera un número de fila aleatorio
(define (randomRowNumber)
  (random 6))

; Función que genera un número de fila aleatorio
(define (randomColumnNumber)
  (random 5))

; Función que inicializa un tablero con las fichas en posiciones aleatorias
; Entrada: Jugador1, Jugador2 y el jugador actual.
; Salida: Tablero con las fichas aleatorias
(define (randomBoard player1 player2 currPlayer)
  (define row (randomRowNumber))
  (define column (randomColumnNumber))
  (define simbolo (player-symbol currPlayer))

  (define anterior (list-set (list-ref matriz row) column simbolo))
  (if (not (countFilledSpaces 0 0 0))
      (begin

        (if (not (formaLinea matriz row column simbolo))
            (begin
              (set! matriz (list-set matriz row anterior))
              (if (equal? currPlayer player1) (set! currPlayer player2) (set! currPlayer player1))
              (randomBoard player1 player2 currPlayer))

            (randomBoard player1 player2 currPlayer)))
      #\
))

;Maquina menu

(define (machine player1 player2 currPlayer)
  (define playIA #\ )
  (define cnd #f)
  (define second #\ )
  (if (equal? (player-symbol currPlayer) "X")
      (begin
        (displayln (string-append "Turno de: X" ))
        (set! cnd (moveValidPieces currPlayer))
            
            )
      (begin
       (displayln "JUGADA DE LA IA:")
      (set! playIA (maxim (list matriz #f ) "O" 50000 (list)))
      (displayln playIA)
      (set! second (maxim playIA "O" 50000 (list)))
      (displayln second)
      (set! matriz (list-ref playIA 0))
       
      
      (set! hashGen (hash))
      (set! cnd #t)
      )
      
      )
  

  (if (equal? cnd #t)
      (if (equal? currPlayer player1) (set! currPlayer player2) (set! currPlayer player1))
      (set! currPlayer currPlayer))

  (display " 0     1     2     3     4   \n")
  (display #\|)
  (displayMat matriz 0 0)
  (if (countSimbolo 0 0 0 "O")
      (displayln "El ganador es X")
      (if (countSimbolo 0 0 0 "X")
          (displayln "El ganador es O")
          (machine player1 player2 currPlayer)))
  )
; Función principal que corre el código.
; Entrada: -
; Salida: Juego de dara.
(define (hagoJugada matrix  fila col currPlayer)
  (define abajo (+ fila 1))
  (define arriba (- fila 1))
  (define izquierda (- col 1))
  (define derecha (+ col 1))
  (define retorno (list))
  
  
  (define lista(append matrix '()))
  (define simbolo currPlayer)
  (when (moveV fila col abajo col currPlayer matrix)
      (define newMat(matrix-set lista  abajo col simbolo))
      (define newMat2(matrix-set newMat  fila col '#\ ))
   
            
    (define h(getHash newMat2 0 0 0 1))
    
    
    (when (not (hash-has-key? hashGen h) )
      (begin
      (set! hashGen  (hash-set hashGen h #t))
      (define forma (formaLinea newMat2 abajo col simbolo))
      (set! retorno (append retorno (list(list newMat2 forma))))
      
      )
      
      
      )
      )
  (when (moveV fila col arriba col currPlayer matrix)
        (define newMat(matrix-set lista arriba col simbolo))
      (define newMat2(matrix-set newMat  fila col '#\ ))
    
            
    (define h(getHash newMat2 0 0 0 1))
    
    
    (when (not (hash-has-key? hashGen h) )
      (begin
      (set! hashGen  (hash-set hashGen h #t))
      (define forma (formaLinea newMat2 arriba col simbolo))
      (set! retorno (append retorno (list(list newMat2 forma))))
      
      )
      
      
      
      )
      )
  (when (moveV fila col fila izquierda currPlayer matrix)
      (define newMat(matrix-set lista  fila izquierda simbolo))
      (define newMat2(matrix-set newMat fila col '#\ ))
    
            
    (define h(getHash newMat2 0 0 0 1))
    
    
    (when (not (hash-has-key? hashGen h) )
      (begin
      (set! hashGen  (hash-set hashGen h #t))
      (define forma (formaLinea newMat2 fila izquierda simbolo))
      (set! retorno (append retorno (list(list newMat2 forma))))
      
      )
      
      
      )
      )
  
  (when (moveV fila col fila derecha currPlayer matrix)
      (define newMat(matrix-set lista  fila derecha simbolo))
      (define newMat2(matrix-set newMat  fila col '#\ ))
    
      
    (define h(getHash newMat2 0 0 0 1))
    
    
    (when (not (hash-has-key? hashGen h) )
      (begin
      (set! hashGen  (hash-set hashGen h #t))
      (define forma (formaLinea newMat2 fila derecha simbolo))
      (set! retorno (append retorno (list(list newMat2 forma))))
     
      
      
      
      
      )
      
      
      )
      )
  
  retorno
  
  
  
  
  
    
  )

(define (children matrix  i j simbolo lista)
  (if (< i (length matrix))
      (begin
    (if(< j (length(list-ref matrix i)))
       (begin
         
        
          (set! lista (append lista (hagoJugada matrix  i j simbolo)))
        
          
         (children matrix  i (+ j 1) simbolo lista)
         
                  
        )
       (children matrix  (+ i 1) 0 simbolo lista)
       
       
       )
    )
      lista
    
      )
  
  
  
  )


(define (jugadaQuitar matrix  i j simbolo)
  (define simbolocontrario simbolo)
  (define retorno (list))
  
  (if (equal? simbolo "O") (set! simbolocontrario "X") (set! simbolocontrario "O"))
  (define m (append (list) matrix))
  (when (equal? (list-ref (list-ref matrix i) j) simbolo)
      (begin
      (set! m (matrix-set m i j '#\ ))
      (set! retorno (append retorno (list(list m #f))))
      )
      
      )
  retorno
  )

(define (hijosQuitarRival matrix  i j simbolo lista )
  (if (< i (length matrix))
      (begin
    (if(< j (length(list-ref matrix i)))
       (begin
         
        
          (set! lista (append lista (jugadaQuitar matrix  i j simbolo)))
        
          
         (hijosQuitarRival matrix  i (+ j 1) simbolo lista)
         
                  
        )
       (hijosQuitarRival matrix  (+ i 1) 0 simbolo lista)
       
       
       )
    )
      lista
    
      )
  )


(define (maxim matrix currPlayer depth ret)
              
  (define simbolocontrario #\ )
  
  (if (equal? currPlayer "O") (set! simbolocontrario "X") (set! simbolocontrario "O"))
  (define count (countSimbolo2 (list-ref matrix 0) 0 0 0 simbolocontrario))
  ; en la posición 1 está si forma una línea o no
  (define formal (list-ref matrix 1))
    (define hijos (list) )
  (if formal
        (set! hijos(hijosQuitarRival (list-ref matrix 0) 0 0 currPlayer '()))   
        (set! hijos(children (list-ref matrix 0) 0 0 currPlayer '()))
                     
        )
  
  (if (and(> count 2) (>(length hijos) 1))
    (begin
    
    
    
    
                                 
                   
    
    
    
  
  
    
               
  
  (map(lambda (x)
                    
                    
                    
        
        
                    (define move (minim x simbolocontrario (- depth 1) ret))
                    (define maax -100000)
                    (define m (max maax (list-ref move 1)) )
        
        
  
                    (define mat ( list-ref move 0))
  
        
                    (set! ret (list mat m (list-ref x 1)))
                    
                 ) hijos)
  
  ret
  )
    
    (list (list-ref matrix 0) (* depth -1) formal)
    
  
  )
  
  
    
  
)
  


(define (minim matrix currPlayer depth ret)
  
  (define simbolocontrario #\ )
  
  
  (if (equal? currPlayer "O") (set! simbolocontrario "X") (set! simbolocontrario "O"))
  (define count (countSimbolo2 (list-ref matrix 0) 0 0 0 simbolocontrario))
  (define formal (list-ref matrix 1))
    (define hijos (list) )
  (if formal
        (set! hijos(hijosQuitarRival (list-ref matrix 0) 0 0 currPlayer '()))   
        (set! hijos(children (list-ref matrix 0) 0 0 currPlayer '()))
                     
        )
  (if (and(> count 2) (>(length hijos) 1))
      (begin
                           
               
  
  (map(lambda (x)  
                    
                    
                    (define move (maxim x simbolocontrario (- depth 1) ret))
        (define miin 100000)
                    (define m (min miin (list-ref move 1)))
                   
                    
  
        
                    (define mat (list-ref move 0))
                    
                    (set! ret (list mat m (list-ref x 1)))
                    
                 ) hijos)
  ret
  )
   (list (list-ref matrix 0) depth  formal)
      
    
   
  
  
  
  )
  
  
  
   )


(define (run-menu)

  (define player1 (player "X" list 0))
  (define player2 (player "O" list 0))
  (define currPlayer player1)
  (define (loop)
    (displayln "Bienvenido al menu")
    (display-menu)
    (define choice (get-user-choice))
    (cond
      [(eq? choice 'option1)

       (playerTurnMenu player1 player2 currPlayer)
       (loop)]
      [(eq? choice 'option2)
       (randomBoard player1 player2 currPlayer)
       (display " 0     1     2     3     4   \n")
       (display #\|)
       (displayMat matriz 0 0)
       (displayln "1. Contra la máquina")
       (displayln "2. PVP")
       (flush-output)
       (define input (read-line))
  
  
       (if (  string=? input "1")
           
           (machine player1 player2 currPlayer)
           (displayPlayMenu player1 player2 currPlayer)
           
           
           )

       (loop)]
      [(eq? choice 'exit)
       (displayln "Gracias por jugar a Dara, Made by Ricardo Soto & Brandon Redondo")]
      [else (loop)]))
  (loop))
(run-menu)
