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


;M-Bool
;evaluates a boolean expression

;comparison -> evaluates a comparison statement with numbers



;M-State format: '((return returnval)(x 0)(y 3)(varname value)...) contains all declared variables
;M-State update functions

;checks to make sure var name is unique, valid (cant be if, while, return or a math symbol) etc etc... need to flesh out more
;(define AddBinding
 ; (lambda (var, val, M-State)
    
  ;  ))

;RemoveBinding
;LookupValue -> enter a variable name, returns the value of that variable
;IsDeclared





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




;****************************Variable Declaration Functions******************************************

;GetVarName -> takes in a variable declaration statement, returns the variable name
(define GetVarName
  (lambda (statement)
    (car (cdr statement))))

;GetFirstBinding -> takes in M-State, returns the first binding
(define GetFirstBinding
  (lambda (M-State)
    (car M-State)))

;GetFirstBindingName -> takes in M-State, returns first variable of first binding
(define GetFirstBindingName
  (lambda (M-State)
    (car (GetFirstBinding M-State))))


;IsNameUnused -> takes in M-State and variable name, makes sure name is not in the list
(define IsNameUnused
  (lambda (M-State name)
    (cond
      [(null? M-State) #t]
      [(eq? (GetFirstBindingName M-State) name) #f]
      [else (IsNameUnused (cdr M-State) name)])))

;HandleVarDec takes in M-State and variable declaration statement, returns updated m state
(define HandleVarDec
  (lambda (M-State statement)
    (cond
      [(IsNameUnused (GetVarName statement)) (AddBinding M-State)]
      []
    )))


;**************************parse tree step through helper functions: ***********************************

;GetFirstStatement - returns the first statement of the parsed program
(define GetFirstStatement
  (lambda (program)
    (if (list? (car program))
     [car program]
     program)))

;IsVarDec takes in a single statement, returns true or false depending on if it is a variable decalaration or not
(define IsVarDec
  (lambda (statement)
    (if (eq? (car statement) 'var)
      #t
      #f)))


;step-through is UNFINISHED
;step-cps takes program: the parsed program, M-state: a list of bindings
;it is used to step through each line of the program
(define step-through
  (lambda (program M-State)
    (cond
      [(null? program) (program)]
      [(IsVarDec (GetFirstStatement program)) (HandleVarDec M-State (GetFirstStatement program))]
      [else program]
    )))


;Main Interpreter function

;Accepts a filename ex: (interpret "testProgram.txt")
;Outputs the return statement of the Simple Program

(define interpret
  (lambda (filename)
    (step-through (parser filename) '())
    ))

;Quick Run:
(interpret "testProgram.txt")


;cps style example
(define factorial-cps
  (lambda (x return)
    (if (zero? x)
        (return 1)
        (factorial-cps (- x 1) (lambda (v) (return (* x v))))
        )))