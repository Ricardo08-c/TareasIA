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
        
         
         
         
         
         
  (display "Ingresa el movimiento, ejemplo : 1 2 ")
  
  
  
  
  (flush-output)
  (define input (read-line))
  (define fila(-(char->integer(string-ref input 0)) 48))
  (define col(-(char->integer(string-ref input 2)) 48))
  
  
  (define volcado(list-set (list-ref matriz fila) col (player-symbol currPlayer)))
  

  (list-set matriz fila volcado)
  
  
  
  
  
        
        
         )





(define(countFilledSpaces i j count)
  (if(>= i (length matriz))
       (>= count 5)
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

(define(displayPlayMenu)
  (0)
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
(displayPlayMenu)
(playerTurnMenu player1 player2 currPlayer)
      )
  
  
  )




(define (randomRowNumber)
  (random 5))

(define (randomColumnNumber)
  (random 6))

(define (randomBoard player1 player2) 

 (displayln "Se rellenó el tablero aleatoriamente")
 (displayln (randomRowNumber))
 (displayln (randomColumnNumber))
  
 )


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
       ;(fillAuto)
       
       (playerTurnMenu player1 player2 currPlayer)
       (loop)]
      [(eq? choice 'option2)
       (randomBoard player1 player2)
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