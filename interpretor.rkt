#lang racket
(require "simpleParser.rkt")

;Orion Follett
;Prithik Karthikeyan

;General Idea:
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



;********************************General Helper Functions********************************

;helper function that flattens a list of lists to just a normal list
(define flatten
  (lambda (lis)
  (cond
    [(null? lis) '()]
    [(pair? lis) (append (flatten (car lis)) (flatten (cdr lis)))]
    [else (list lis)]
    )))

;takes a list of lists, replaces all elements equal to t with r, returns new list
(define replaceall*
  (lambda (t r lis)
    (cond
      [(null? lis) '()]
      [(pair? (car lis)) (cons (replaceall* t r (car lis)) (replaceall* t r (cdr lis)))]
      [(eq? t (car lis)) (cons r (replaceall* t r (cdr lis)))]
      [else (cons (car lis) (replaceall* t r (cdr lis)))]
      )))

;takes an atom, returns true if it is an integer operation
(define int-operation?
  (lambda (x)
    (if (or (eq? x '+) (eq? x '-) (eq? x '/) (eq? x '*) (eq? x '%))
        #t
        #f
    )))

;takes an atom, returns true if it is a bool operation
(define bool-operation?
  (lambda (x)
    (if (or (eq? x '>) (eq? x '<) (eq? x '==) (eq? x '<=) (eq? x '>=) (eq? x '!=) (eq? x '&&) (eq? x '||) (eq? x '!))
        #t
        #f
    )))

;takes an atom, returns true if it is an integer or a boolean operation
(define operation?
  (lambda (x)
    (if (or (bool-operation? x) (int-operation? x))
        #t
        #f
    )))

;*******************************HELPER FUNCTIONS FOR M_INTEGER**************************

;MI_GetOperation means it is a helper function only to be used with M-Integer
;get-operation accepts a list in prefix notation
;Extracts the operation from the expression
;Ex: (get-operation '(+ 1 2)) -> +
(define MI_GetOperation
  (lambda (expression)
    (car expression)
    ))

;MI_GetFirstOperand accepts a list in prefix notation
;Extracts the first operand from the expression
;Ex: (get-operation '(/ 1 2)) -> 1
(define MI_GetFirstOperand
  (lambda (expression)
    (car (cdr expression))
    ))

;MI_GetSecondOperand accepts a list in prefix notation
;Extracts the first operand from the expression
;Ex: (get-operation '(/ 1 2)) -> 2
(define MI_GetSecondOperand
  (lambda (expression)
    (car (cdr (cdr expression)))))

(define MI_IsUnary
  (lambda (expression)
    (not (pair? (cdr (cdr expression))))
    ))
    
;M-Integer: evaluates integer expressions, takes in an expression, outputs an integer value
;takes form '(operation number number) possible operations:  + | - | * | / | %
;EX: (M_integer '(+ (- 1 2) (* 4 5))) -> 19

(define M-Integer
  (lambda (expression)
    (cond
      [(number? expression) expression]
      [(and (MI_IsUnary expression)(eq? (MI_GetOperation expression) '-)) (* -1 (M-Integer (MI_GetFirstOperand expression)))]
      [(eq? (MI_GetOperation expression) '+) (+ (M-Integer(MI_GetFirstOperand expression)) (M-Integer(MI_GetSecondOperand expression)))]
      [(eq? (MI_GetOperation expression) '-) (- (M-Integer(MI_GetFirstOperand expression)) (M-Integer(MI_GetSecondOperand expression)))]
      [(eq? (MI_GetOperation expression) '*) (* (M-Integer(MI_GetFirstOperand expression)) (M-Integer(MI_GetSecondOperand expression)))]
      [(eq? (MI_GetOperation expression) '/) (quotient (M-Integer(MI_GetFirstOperand expression)) (M-Integer(MI_GetSecondOperand expression)))]
      [(eq? (MI_GetOperation expression) '%) (remainder (M-Integer(MI_GetFirstOperand expression)) (M-Integer(MI_GetSecondOperand expression)))])))

;M-Bool
;evaluates a boolean expression that has comparison operators, and logical operators




;***********************************M-Value Helper Functions*****************************************

;there are two types of expressions, boolean expressions, and integer expressions
;MV_IsBoolExpression -> takes in a flattened expression, if the expression has any of the boolean operators in it return true, else return false
(define MV_IsBoolExpression
  (lambda (expression)
    (cond
      [(null? expression) #f]
      [(bool-operation? (car expression)) #t]
      [else (MV_IsBoolExpression (cdr expression))])))

;MV_ListOfVars -> takes fexpression: a flattened integer expression that may have variables in it, returns a flattened list of all the variables within that expression
(define MV_ListOfVars
  (lambda (fexpression)
    (cond
      [(null? fexpression) fexpression]
      [(or (number? (car fexpression)) (operation? (car fexpression))) (MV_ListOfVars (cdr fexpression))]
      [else (cons (car fexpression) (MV_ListOfVars (cdr fexpression)))])))

;MV_ConvertVarToVal* -> takes M-State, expression: an integer expression that may have variables in it, and varList: a list of all the variables in the expression
;converts all the variables into values and returns the list
(define MV_ConvertVarToVal*
  (lambda (M-State expression varList)
    (cond
      [(null? varList) expression]
      [(IsVarUndeclared M-State (car varList)) (error expression " variable not defined in an expression variable name: " (car varList))]
      [else (MV_ConvertVarToVal* M-State (replaceall* (car varList) (LookupValue M-State (car varList)) expression) (cdr varList))])))

;M-Value -> takes in M-State and a partial statement, ultimately resolves the partial statement down to a value and returns that value could be true, false, or a number
(define M-Value
  (lambda (M-State val)
    (cond
      [(or (number? val) (eq? val 'null) (eq? val #t) (eq? val #f)) val]
      [(and (list? val) (MV_IsBoolExpression (flatten val))) (error val "No M-Value for boolean expressions yet")] ;potential mix of integer and comparison operators
      [(list? val) (M-Integer (MV_ConvertVarToVal* M-State val (MV_ListOfVars (flatten val))))] ;Integer expression only
      [(IsVarUndeclared M-State val) (error val "Undeclared variable!")] ;undeclared variable
      [else (LookupValue M-State val)])))

;*************************M-State Helper Functions**************************

;M-State format: '((return returnval)(x 0)(y 3)(varname value)...) contains all declared variables
;LookupValue -> enter a variable name, returns the value of that variable


;GetFirstBinding -> takes in M-State, returns the first binding
(define GetFirstBinding
  (lambda (M-State)
    (car M-State)))

;GetFirstBindingName -> takes in M-State, returns first variable name of first binding
(define GetFirstBindingName
  (lambda (M-State)
    (car (GetFirstBinding M-State))))

;GetFirstBindingValue -> takes in M-State, returns first variable value of first binding
(define GetFirstBindingValue
  (lambda (M-State)
    (car (cdr (GetFirstBinding M-State)))))

;IsNameUnused -> takes in M-State and variable name, makes sure name is not in the list
(define IsVarUndeclared
  (lambda (M-State name)
    (cond
      [(null? M-State) #t]
      [(eq? (GetFirstBindingName M-State) name) #f]
      [else (IsVarUndeclared (cdr M-State) name)])))

;AddNewBinding -> takes in M-State, variable name, variable value, returns M-State with new binding
(define AddNewBinding
  (lambda (M-State varName varVal)
    (cons (list varName (M-Value M-State varVal)) M-State)))

;RemoveBinding -> takes in M-State, variable name returns M-State without that variable
;If the binding doesnt exist, M-State is unchanged
(define RemoveBinding
  (lambda (M-State varName)
    (cond
     [(null? M-State) M-State]
     [(eq? (GetFirstBindingName M-State) varName) (cdr M-State)]
     [else (cons (GetFirstBinding M-State) (RemoveBinding (cdr M-State) varName))])))

;ChangeBinding -> takes in M-State, variable name, new variable value, returns M-State with old variable value replaced by new variable value
;If the binding doesnt exist, it creates a new one
(define ChangeBinding
  (lambda (M-State varName varVal)
    (AddNewBinding (RemoveBinding M-State varName) varName varVal)))

;LookupValue -> takes in M-State, variable name, returns the value associated with that variable
;returns 'null if there is no value associated with that variable
(define LookupValue
  (lambda (M-State varName)
    (cond
      [(null? M-State) 'null]
      [(eq? (GetFirstBindingName M-State) varName) (GetFirstBindingValue M-State)]
      [else (LookupValue (cdr M-State) varName)])))


;****************************Variable Declaration Functions******************************************
;Variable declaration helper functions specific to variable declaration statements are marked VD_

;GetVarName -> takes in a variable declaration statement, returns the variable name
(define VD_GetVarName
  (lambda (statement)
    (car (cdr statement))))

;GetVarValue -> takes in a variable declaration statement, returns the value it is assigned to, if there is no value, return 'null for the value
(define VD_GetVarValue
  (lambda (statement)
    (cond
      [(null? (cdr (cdr statement))) 'null]
      [else (car (cdr (cdr statement)))])))

;HandleVarDec takes in M-State and variable declaration statement, returns updated m state
(define HandleVarDec
  (lambda (M-State statement)
    (cond
      [(IsVarUndeclared M-State (VD_GetVarName statement)) (AddNewBinding M-State (VD_GetVarName statement) (VD_GetVarValue statement))]
      [else (error "Error: " (VD_GetVarName statement) "variable already declared")]
    )))

;****************************Assignment Statement Functions******************************************

;AS_ indicated an assignment statement helper function, these should only be used with assignment statements

;AS_GetVarName -> takes in an assignment statement, returns the variable name being assigned
(define AS_GetVarName
  (lambda (statement)
    (car (cdr statement))))

;AS_GetVarVal -> takes in an assignment statement, returns the variable value in the statement
;VarVal could be another variable, a math statement, or 
(define AS_GetVarVal
  (lambda (statement)
    (car (cdr (cdr statement)))))
  
;HandleAssign -> Takes in M-State and an assignment statement, returns updated M-State
(define HandleAssign
  (lambda (M-State statement)
    (cond
      [(IsVarUndeclared M-State (AS_GetVarName statement)) (error (AS_GetVarName statement) "Assignment before declaration!")]
      [else (ChangeBinding M-State (AS_GetVarName statement) (AS_GetVarVal statement))])))

;**************************parse tree step through helper functions: ***********************************

;GetFirstStatement - returns the first statement of the parsed program
(define GetFirstStatement
  (lambda (program)
    (if (list? (car program))
     [car program]
     program)))

;IsVarDecStatement takes in a single statement, returns true or false depending on if it is a variable decalaration or not
(define IsVarDecStatement
  (lambda (statement)
    (if (eq? (car statement) 'var)
      #t
      #f)))


;IsAssignStatement takes in a single statement, returns true or false depending on if it is an assignment
(define IsAssignStatement
  (lambda (statement)
    (if (eq? (car statement) '=)
        #t
        #f)))


;step-through is UNFINISHED
;step-cps takes program: the parsed program, M-state: a list of bindings
;it is used to step through each line of the program
(define step-through
  (lambda (program M-State)
    (cond
      [(null? program) M-State]
      [(IsVarDecStatement (GetFirstStatement program)) (step-through (cdr program) (HandleVarDec M-State (GetFirstStatement program)))]
      [(IsAssignStatement (GetFirstStatement program)) (step-through (cdr program) (HandleAssign M-State (GetFirstStatement program)))]
      [else M-State])))


;Main Interpreter function

;Accepts a filename ex: (interpret "testProgram.txt")
;Outputs the return statement of the Simple Program

(define interpret
  (lambda (filename)
    (step-through (parser filename) '())))

;Quick Run:
(interpret "testProgram.txt")


;cps style example
(define factorial-cps
  (lambda (x return)
    (if (zero? x)
        (return 1)
        (factorial-cps (- x 1) (lambda (v) (return (* x v))))
        )))