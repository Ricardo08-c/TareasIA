#lang racket
 (define my-player (player "Alice" '(move1 move2) 10))

(define (display-menu)
  (displayln "Menu:")
  (displayln "1. Option 1")
  (displayln "2. Option 2")
  (displayln "3. Option 3")
  (displayln "4. Exit"))

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



  
(define (run-menu)


  (define (loop)
    (displayln "Welcome to the menu!")
    (display-menu)
    (define choice (get-user-choice))
    (cond
      [(eq? choice 'option1)
       (displayln "You chose Option 1.")
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