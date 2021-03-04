#lang racket
(require "simpleParser.rkt")






;Abstractions

;HELPER FUNCTIONS FOR M_INTEGER
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
    
;M-Integer: evaluates integer expressions
;takes form '(operation number number) possible operations:  + | - | * | / | %
;EX: (M_integer '(+ (- 1 2) (* 4 5))) -> 19

(define M_integer
  (lambda (expression)
    (cond
      [(number? expression) expression]
      [(eq? (get-operation expression) '+) (+ (M_integer(get-first-operand expression)) (M_integer(get-second-operand expression)))]
      [(eq? (get-operation expression) '-) (- (M_integer(get-first-operand expression)) (M_integer(get-second-operand expression)))]
      [(eq? (get-operation expression) '*) (* (M_integer(get-first-operand expression)) (M_integer(get-second-operand expression)))]
      [(eq? (get-operation expression) '/) (quotient (M_integer(get-first-operand expression)) (M_integer(get-second-operand expression)))]
      [(eq? (get-operation expression) '%) (remainder (M_integer(get-first-operand expression)) (M_integer(get-second-operand expression)))])))



;parser filename gives a list where each sublist is a statement
;there are five different types of statements

;variable declaration	(var variable) or (var variable value)
;assignment	(= variable expression)
;return	(return expression)
;if statement	(if conditional then-statement optional-else-statement)
;while statement	(while conditional body-statement)

;interpret will step through each statement, execute what needs to get done based on that line, and then interpret the rest of the code

;There may need to be an M-state list that is passed nearly everywhere, it will have all variable bindings so '((x 1) (y 3) ...)

;return statement indicates end of the program

;interpretor should probably loop using continuation passing style rather than regular recursion of the cdr of the list


;step-cps is UNFINISHED
;step-cps takes program: the parsed program, M-state: a list of bindings, and return a continuation passing function
;it is used to step through each line of the program
(define step-cps
  (lambda (program M-state return)
    (cond
      [(null? program) (return program)]
      [else (return program)]
    )))


;Main Interpreter function

;Accepts a filename ex: (interpret "testProgram.txt")
;Outputs the return statement of the Simple Program

(define interpret
  (lambda (filename)
    (step-cps (parser filename) '() (lambda(v) v))
    ))

;Quick Run:
;(interpret "testProgram.txt")


;cps style example
(define factorial-cps
  (lambda (x return)
    (if (zero? x)
        (return 1)
        (factorial-cps (- x 1) (lambda (v) (return (* x v))))
        )))