#lang racket
(require "simpleParser.rkt")






;Abstractions

;get-operation accepts a list in prefix notation
;Extracts the operation from the expression
;Ex: (get-operation '(+ 1 2)) -> +
(define get-operation
  (lambda (expression)
    (car expression)
    ))

;get-first-operand accepts a list in prefix notation
;Extracts the first operand from the expression
;Ex: (get-operation '(/ 1 2)) -> 1
(define get-first-operand
  (lambda (expression)
    (car (cdr expression))
    ))


;get-second-operand accepts a list in prefix notation
;Extracts the first operand from the expression
;Ex: (get-operation '(/ 1 2)) -> 2
(define get-second-operand
  (lambda (expression)
    (car (cdr (cdr expression)))))
    
;M-Integer
;takes form '(number operation number) possible operations:  + | - | * | / | %

(define M_integer
  (lambda (expression)
    (cond
      [(number? expression) expression]
      [(eq? (get-operation expression) '+) (+ (M_integer(get-first-operand expression)) (M_integer(get-second-operand expression)))]
      [(eq? (get-operation expression) '-) (- (M_integer(get-first-operand expression)) (M_integer(get-second-operand expression)))]
      [(eq? (get-operation expression) '*) (* (M_integer(get-first-operand expression)) (M_integer(get-second-operand expression)))]
      [(eq? (get-operation expression) '/) (quotient (M_integer(get-first-operand expression)) (M_integer(get-second-operand expression)))]
      [(eq? (get-operation expression) '%) (remainder (M_integer(get-first-operand expression)) (M_integer(get-second-operand expression)))])))


;Main Interpreter function

;Accepts a filename ex: (interpret "testProgram.txt")
;Outputs the return statement of the Simple Program

(define interpret
  (lambda (filename)
    (parser filename)))

;Quick Run:
;(interpret "testProgram.txt")
