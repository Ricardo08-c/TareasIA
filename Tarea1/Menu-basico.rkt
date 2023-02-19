#lang racket

(struct player (symbol moves moveQuant)#:mutable) 
(define matriz(build-list 6 (lambda (i)
                 (build-list 5 (lambda (j) '#\ )))))
(define (display-menu)
  (displayln "Menu:")
  (displayln "1. Llenar manualmente")
  (displayln "2. Llenar aleatoriamente")
  (displayln "4. Salir")
)
(define (get-user-choice)
  
  
  (display "Enter your choice: ")
  (flush-output)
  (define input (read-line))
  (cond
    [(string=? input "1") 'option1]
    [(string=? input "2") 'option2]
    [(string=? input "3") 'option3]
    [(string=? input "4") 'exit]
    [else
     (displayln "Invalid choice, please try again.")
     (get-user-choice)]))

(define (displayMat matrix i j)
    (if(>= i (length matrix))
       (display #\newline )
       (if(< j(length (list-ref matrix i)))
       (begin
          (display (list-ref(list-ref matrix i) j))
         (display #\ )
         (display #\ )
         (display #\| )
         
         (display #\ )
         (display #\ )
         (displayMat matrix i (+ j 1)))

       (begin
         
         (display #\newline )
         (display #\| )
         
         
         (displayMat matrix (+ i 1) 0))
       
        
       
       ))
      
     
       )
         
      
 
  
(define (movePieces matriz currPlayer)
        
         
         
         
         
         
  (display "Ingresa la ficha, ejemplo : 1 2  ")
  
  
  
  
  (flush-output)
  (define input (read-line))
  (define fila(-(char->integer(string-ref input 0)) 48))
  (define col(-(char->integer(string-ref input 2)) 48))
  
  
  (define volcado(list-set (list-ref matriz fila) col (player-symbol currPlayer)))
  

  (list-set matriz fila volcado)
  
  
  
  
  
        
        
         )





(define(countFilledSpaces i j count)
  (if(>= i (length matriz))
       (>= count 24)
       (if(< j(length (list-ref matriz i)))
       (begin
          (if(not(equal? (list-ref(list-ref matriz i) j) '#\ ))
          (countFilledSpaces  i (+ j 1) (+ count 1 ))
          (countFilledSpaces  i (+ j 1) count)
          )
          
          
       
         )
         

       (begin
                  
         
         
         (countFilledSpaces (+ i 1) 0 count) )
       
        
       
       ))
  )

(define (cogerFichaEnemiga currPlayer)
    (displayln #\ )
  
               
    (displayln "¡Lograste alinear 3 elementos, coje una ficha enemiga! Ingresa la ficha, ejemplo : 1 2 ")
  
  (flush-output)
  (define input (read-line))
  (define fila(-(char->integer(string-ref input 0)) 48))
  (define col(-(char->integer(string-ref input 2)) 48))
  
  ; validar que la ficha sea la contraria a currPlayer
  
  (define volcado(list-set (list-ref matriz fila) col #\ ))
  

  (list-set matriz fila volcado)

  
  
  )
(define(displayPlayMenu player1 player2 currPlayer)
    
 
  (displayln (string-append "Turno de: " (player-symbol currPlayer))  )
  (define cnd(movePiecesGame  currPlayer))
  
  (if(equal? cnd #t)
     (if(equal? currPlayer player1)
     (set! currPlayer player2)
     (set! currPlayer player1)
     )
     (set! currPlayer currPlayer)
     
     
     
     
     )
  (if(> 3  2)
     (cogerFichaEnemiga currPlayer)
     #\
     )
  (display #\| )
  
       (displayMat matriz 0 0)
  
  (displayPlayMenu  player1 player2 currPlayer)
  )


(define(moveIsValid rowIn colIn rowDes colDes currPlayer )
   (define cond1(equal?(player-symbol currPlayer) (list-ref(list-ref matriz rowIn) colIn)))
  (define cond2(equal?(list-ref(list-ref matriz rowDes) colDes) #\ ))
  (define rowMove(and(= (abs (- rowIn rowDes)) 1) (= colIn colDes)))
  (define colMove(and(= (abs (- colIn colDes)) 1) (= rowIn rowDes)))
  


  
  (and cond1 cond2 (or rowMove colMove))
 
     
  
  )




(define (movePiecesGame  currPlayer)
        
         
         
         
         
         
  (display "Ingresa el movimiento, debe respetar los espacios, sintáxis: $/1 2 -> 3 2 ")
  
  
  
  
  (flush-output)
  (define input (read-line))
  (define filaIn(-(char->integer(string-ref input 0)) 48))
  (define colIn(-(char->integer(string-ref input 2)) 48))
  (define filaDes(-(char->integer(string-ref input 7)) 48))
  (define colDes(-(char->integer(string-ref input 9)) 48))

  
  
  (define simbolo(player-symbol currPlayer))
  
  (define volcado(list-set (list-ref matriz filaDes) colDes (list-ref(list-ref matriz filaIn) colIn)))
  (define cond (moveIsValid filaIn colIn filaDes colDes currPlayer))
  (if cond      
      
      (set! matriz(list-set matriz filaDes volcado))
           
      #\
      )
  (define anterior(list-set (list-ref matriz filaIn) colIn #\ ))
  
  
  
  ; Aqui se debe validar el movimiento (que no hayan fichas donde lo mueve, que haga un 3 en línea
  
  (if cond
      (begin
        (set! matriz(list-set matriz filaIn anterior))
        #t
      
      
      
      
      
      )
      
      (begin
        (writeln #\ )
        (writeln "XXXXXXXXXXXXXX" )
            (writeln "Movimiento inválido")
            (writeln "XXXXXXXXXXXXXX" )
            (writeln #\ )
            #f
            
            )
      )
      
      

 
  
  
  
  
  
        
        
         )




(define (playerTurnMenu player1 player2 currPlayer)
  
  (if(equal? currPlayer player1)
     (set! currPlayer player2)
     (set! currPlayer player1)
     )
  (displayln (string-append "Turno de: " (player-symbol currPlayer))  )
  
  (set! matriz (movePieces matriz currPlayer))

  (display #\| )
  
       (displayMat matriz 0 0)
  
  (if (countFilledSpaces 0 0 0)
      (begin
        (writeln "Inicio del juego!")
(displayPlayMenu currPlayer player1 player2)

)
(playerTurnMenu player1 player2 currPlayer)
      )
  
  
  )
(define (formaLinea fila columna caracter)
  #t
  )
(define (randomRowNumber)
  (random 6))

(define (randomColumnNumber)
  (random 5))

(define (randomBoard player1 player2 currPlayer)
   (define row (randomRowNumber))
   (define column (randomColumnNumber))
  (define simbolo(player-symbol currPlayer))
  
  (display #\|)
  (displayMat matriz 0 0)
  (define anterior(list-set (list-ref matriz row) column simbolo))
  (if (not(countFilledSpaces 0 0 0))
      (begin
        (displayln "Se rellenó el tablero aleatoriamente")
      (if(not(formaLinea row column simbolo))
         (begin
              (set! matriz(list-set matriz row anterior))
               (if(equal? currPlayer player1)
                  (set! currPlayer player2)
                  (set! currPlayer player1)
                  )
               (randomBoard player1 player2 currPlayer)
               )
         
         (randomBoard player1 player2 currPlayer)
         
         )
        
        
       

)
#\
      )
 

  


  
 (displayln row)
 (displayln column)

 )

(define (transpose matrix)
  (apply map list matrix))


(define (no-hay-3-repeticiones? matriz)
  (and (no-hay-repeticiones-verticales? matriz)
       (no-hay-repeticiones-horizontales? matriz)))

(define (no-hay-repeticiones-verticales? matriz)
  (not (existen-3-repeticiones? (transpose matriz))))

(define (no-hay-repeticiones-horizontales? matriz)
  (not (existen-3-repeticiones? matriz)))

(define (existen-3-repeticiones? matriz)
  (ormap (lambda (fila) (existen-3-repeticiones-aux? fila 0))
         matriz
         (transpose matriz)))

(define (existen-3-repeticiones-aux? fila n)
  (cond ((>= n (- (length fila) 2)) #f)
        ((and (equal? (list-ref fila n) (list-ref fila (+ n 1)))
              (equal? (list-ref fila n) (list-ref fila (+ n 2))))
         #t)
        (else (existen-3-repeticiones-aux? fila (+ n 1)))))



(define (run-menu)

  (define player1 (player "X" list 0))
  (define player2 (player "O" list 0))
  (define currPlayer player1)
  (define (loop)
    (displayln "Welcome to the menu!")
    (display-menu)
    (define choice (get-user-choice))
    (cond
      [(eq? choice 'option1)
       
       
       (playerTurnMenu player1 player2 currPlayer)
       (loop)]
      [(eq? choice 'option2)
       (randomBoard player1 player2 currPlayer)
       (loop)]
      [(eq? choice 'option3)
       (displayln "You chose Option 3.")
       (loop)]
      [(eq? choice 'exit)
       (displayln "Goodbye!")]
      [else
       (loop)]))
  (loop))
(run-menu)