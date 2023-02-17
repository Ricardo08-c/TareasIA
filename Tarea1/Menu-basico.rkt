#lang racket
(struct player (symbol moves moveQuant)#:mutable) 

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
         
      
 
  
(define (movePieces p1 p2 matriz)
        
         
         
         
         
         
         (display "Ingresa el movimiento, ejemplo : 1 2 ")
  
  
  
  
  (flush-output)
  (define input (read-line))
  (define fila(-(char->integer(string-ref input 0)) 48))
  (define col(-(char->integer(string-ref input 2)) 48))
  
  
  (define volcado(list-set (list-ref matriz fila) col "X"))

  (list-set matriz fila volcado)
  
  
  
  
  
        
        
         )



(define matriz(build-list 6 (lambda (i)
                 (build-list 5 (lambda (j) 0)))))


(define (playerTurnMenu player1 player2)
  (set! matriz (movePieces player1 player2 matriz))
       (display #\| )
       (displayMat matriz 0 0)
  (playerTurnMenu player1 player2)
  )
(define (run-menu)

  (define player1 (player "X" list 0))
  (define player2 (player "O" list 0))
  (define (loop)
    (displayln "Welcome to the menu!")
    (display-menu)
    (define choice (get-user-choice))
    (cond
      [(eq? choice 'option1)
       ;(fillAuto)
       
       (playerTurnMenu player1 player2)
       (loop)]
      [(eq? choice 'option2)
       (displayln "You chose Option 2.")
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