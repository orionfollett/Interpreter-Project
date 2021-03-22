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

;There is an M-state list that is passed nearly everywhere, it will have all variable bindings so '((x 1) (y 3) ...)

;return statement indicates end of the program, M-State gets changed to (return value) and the program returns the value



;********************************General Helper Functions********************************

;append but cps style
(define append-cps
  (lambda (l1 l2 return)
    (if (null? l1)
        (return l2)
        (append-cps (cdr l1) l2 (lambda(v) (return (cons (car l1) v))))
       )))

;helper function that flattens a list of lists to just a normal list
(define flatten
  (lambda (lis)
   (flatten-cps lis (lambda(v) v))))

;cps style flatten
(define flatten-cps
  (lambda (lis return)
  (cond
    [(null? lis) (return lis)]
    [(pair? lis) (flatten-cps (cdr lis)
                              (lambda(v1) (flatten-cps (car lis)
                                                       (lambda(v2) (append-cps v2 v1 return)))))]
    [else (return (list lis))])))

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

;contains-bool -> takes in a flattened expression, if the expression has any of the boolean operators in it return true, else return false
(define contains-bool-operator
  (lambda (expression)
    (cond
      [(null? expression) #f]
      [(bool-operation? (car expression)) #t]
      [else (contains-bool-operator (cdr expression))])))

;var? -> returns true if x is not a keyword (true or false), operation, or number 
(define var?
  (lambda (x)
     (not (or (operation? x) (eq? x 'true)  (eq? x 'false) (number? x)))
    ))


;custom-bool-literal? -> takes an atom, returns #t if it is a valid bool literal
(define custom-bool-literal?
  (lambda (x)
    (or (eq? x 'true) (eq? x 'false))
    ))

;bool? -> takes an atom, returns #t if it is either #t or #f, returns false if it isnt
(define bool?
  (lambda (x)
    (cond
      [(or (eq? x #t) (eq? x #f)) #t]
      [else #f]
    )))

;ConvertToSchemeBool - takes val that is either 'true or 'false, returns #t if it is 'true, #f is it is 'false
(define ConvertToSchemeBool
  (lambda (val)
    (cond
      [(eq? val 'true) #t]
      [(eq? val 'false) #f]
      [else (error "Value that is not a bool trying to be converted into a bool!")]
    )))

;ConvertToCustomBool - takes val that is either #t or #f converts it to 'true or 'false
(define ConvertToCustomBool
  (lambda (val)
    (cond
      [(eq? val #t) 'true]
      [(eq? val #f) 'false]
      [else (error "Value that is not a bool trying to be converted into a bool!")]
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
      [(eq? (MI_GetOperation expression) '%) (remainder (M-Integer(MI_GetFirstOperand expression)) (M-Integer(MI_GetSecondOperand expression)))]
      [else (M-Bool expression)])))

;M-Bool
;evaluates a boolean expression that has comparison operators, and logical operators, outputs true or false
;takes form '(operation expression expression) possible operations:  && | || | ! |
;EX: (M_Bool '(true || false)) -> true

(define M-Bool
  (lambda (expression)
    (cond
      [(bool? expression) expression]
      [(custom-bool-literal? expression) (ConvertToSchemeBool expression)]
      [(and (MI_IsUnary expression)(eq? (MI_GetOperation expression) '!)) (not (M-Bool (MI_GetFirstOperand expression)))]
      [(eq? (MI_GetOperation expression) '&&) (and (M-Bool(MI_GetFirstOperand expression)) (M-Bool(MI_GetSecondOperand expression)))]
      [(eq? (MI_GetOperation expression) '||) (or (M-Bool(MI_GetFirstOperand expression)) (M-Bool(MI_GetSecondOperand expression)))]
      ;[else (error "non bool tried to use boolean operators")])))
      [else (M-Compare expression)])))
;M-Compare
;evaluates a comparison expression that has comparison operators numbers, and integer expressions, outputs true or false
;takes form '(operation expression expression) possible operations:  
;< | > | == | <= | >= | !=
;EX: (M_Compare '(> 2 (+ 3 1))) -> false

(define M-Compare
  (lambda (expression)
    (cond
      [(eq? (MI_GetOperation expression) '>) (> (M-Integer(MI_GetFirstOperand expression)) (M-Integer(MI_GetSecondOperand expression)))]
      [(eq? (MI_GetOperation expression) '<) (< (M-Integer(MI_GetFirstOperand expression)) (M-Integer(MI_GetSecondOperand expression)))]
      [(eq? (MI_GetOperation expression) '>=) (>= (M-Integer(MI_GetFirstOperand expression)) (M-Integer(MI_GetSecondOperand expression)))]
      [(eq? (MI_GetOperation expression) '<=) (<= (M-Integer(MI_GetFirstOperand expression)) (M-Integer(MI_GetSecondOperand expression)))]
      [(eq? (MI_GetOperation expression) '==) (eq? (M-Integer(MI_GetFirstOperand expression)) (M-Integer(MI_GetSecondOperand expression)))]
      [(eq? (MI_GetOperation expression) '!=) (not (eq? (M-Integer(MI_GetFirstOperand expression)) (M-Integer(MI_GetSecondOperand expression))))]
      [else (error "No way to resolve expression!")])))

;Begins entry point for expression evaluation
(define M-Expression
  (lambda (expression)
    (M-Integer expression)
    ))

;***********************************M-Value Helper Functions*****************************************

;there are three types of expressions, compare expressions, boolean expressions, and integer expressions

;MV_ListOfVars -> takes fexpression: a flattened integer expression that may have variables in it, returns a flattened list of all the variables within that expression
(define MV_ListOfVars
  (lambda (fexpression)
    (cond
      [(null? fexpression) fexpression]
      [(not (var? (car fexpression))) (MV_ListOfVars (cdr fexpression))]
      [else (cons (car fexpression) (MV_ListOfVars (cdr fexpression)))])))

;MV_ConvertVarToVal* -> takes M-State, expression: an integer expression that may have variables in it, and varList: a list of all the variables in the expression
;converts all the variables into values and returns the list
(define MV_ConvertVarToVal*
  (lambda (M-State expression varList)
    (cond
      [(null? varList) expression]
      [(IsVarUndeclared M-State (car varList)) (error " variable not defined in an expression variable name: " (car varList))]
      [else (MV_ConvertVarToVal* M-State (replaceall* (car varList) (LookupValue M-State (car varList)) expression) (cdr varList))])))

;MV_NoProcessingNeeded - takes in val, returns true if it is a value, false if it needs further processing
(define MV_NoProcessingNeeded
  (lambda (val)
    (or (number? val) (eq? val 'null) (eq? val #t) (eq? val #f))
    ))

;MV_IsBoolExpression - takes a val, returns true if it is an expression with bool operators
(define MV_IsBoolExpression
  (lambda (val)
    (and (list? val) (contains-bool-operator (flatten val)))
    ))

;M-Value -> takes in M-State and a partial statement, ultimately resolves the partial statement down to a value and returns that value could be true, false, or a number
(define M-Value
  (lambda (M-State val)
    (cond
      [(MV_NoProcessingNeeded val) val]
      [(list? val) (M-Expression (MV_ConvertVarToVal* M-State val (MV_ListOfVars (flatten val))))] ;Evaluate the expression
      [(custom-bool-literal? val) (ConvertToSchemeBool val)]
      [(IsVarUndeclared M-State val) (error "Undeclared variable!" val)] ;undeclared variable
      [else (LookupValue M-State val)]))) ;declared variable that needs to be resolved to a value


;*************************M-State Helper Functions**************************

;M-State format: '((return returnval)(x 0)(y 3)(varname value)...) contains all declared variables

;GetFirstBinding -> takes in M-State, returns the first binding
(define GetFirstBinding
  (lambda (M-State)
    (cond
      [(null? M-State) '()]
      [else (car M-State)])))

;GetFirstBindingName -> takes in M-State, returns first variable name of first binding
(define GetFirstBindingName
  (lambda (M-State)
    (cond
     [(null? (GetFirstBinding M-State)) '()]
     [else (car (GetFirstBinding M-State))])))

;GetFirstBindingValue -> takes in M-State, returns first variable value of first binding
(define GetFirstBindingValue
  (lambda (M-State)
    (cond
      [(null? M-State) '()]
      [else (car (cdr (GetFirstBinding M-State)))])))

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

;AS_ indicates an assignment statement helper function, these should only be used with assignment statements

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
      [else (ChangeBinding M-State (AS_GetVarName statement) (M-Value M-State (AS_GetVarVal statement)))])))


;****************************If Statement functions****************************************************

;I_ indicates it is a helper function for HandleIf, it should only be used for HandleIf

;I_GetIfCondition takes in if statement returns if condition
(define I_GetIfCondition
  (lambda (statement)
    (car (cdr statement))))

;I_GetIfBody takes in if statement returns if body
(define I_GetIfBody
  (lambda (statement)
    (car (cdr (cdr statement)))))

;I_GetNext takes in if statement returns next if condition (should return either the else body or an empty list if there isnt one)
(define I_GetNext
  (lambda (statement)
    (cond
      [(null? statement) '()]
      [(null? (cdr (cdr (cdr statement)))) '()]
      [else (car (cdr (cdr (cdr statement))))]
    )))

;I_IsIf? takes a statement, returns true if the beginning is a full if block
(define I_IsIf?
  (lambda (statement)
    (eq? (car statement) 'if)
    ))

;HandleIf -> Takes in M-State and an if statement, returns updated M-State
(define HandleIf
  (lambda (M-State statement)
    (cond
    [(not (list? M-State)) (list (list 'return M-State))];program returned during the if statement, program returns a single value so put it back in proper form
    [(null? statement) M-State];none of the if statements were true
    [(M-Value M-State (I_GetIfCondition statement)) (HandleIf (step-through (I_GetIfBody statement) M-State) '())] ;if statement was true, so run the body
    [(and (not (null? (I_GetNext statement))) (I_IsIf? (I_GetNext statement))) (HandleIf M-State (I_GetNext statement))] ;if statement was false, but there are more ifs to check check the next one
    [(null? (I_GetNext statement)) M-State];nothing left to check, return the state
    [else (HandleIf (step-through (I_GetNext statement) M-State) '())];there is an else statement remaining, run that code, then return the updated M-State
    )))


;*****************************While Statement functions ******************************************

;W_ indicates it is a helper function for HandleWhile so it should only be used for that
;W_GetWhileCondition takes in a while statement and returns the loop condition
(define W_GetWhileCondition
  (lambda (statement)
    (car (cdr statement))
    ))

;W_CheckWhileCondition takes in a while statement and return true or false depending on if it is true or false
(define W_CheckWhileCondition
  (lambda (M-State statement)
    (M-Value M-State (W_GetWhileCondition statement))
    ))

;W_GetWhileBody takes in a while statement and returns the body of the loop
(define W_GetWhileBody
  (lambda (statement)
    (car (cdr (cdr statement)))
    ))

;HandleWhile -> Takes in M-State and a while statement, returns updated M-State
(define HandleWhile
  (lambda (M-State statement)
   (cond
    [(not (list? M-State)) (list (list 'return M-State))];program returned during the loop, program returns a single value so put it back in proper form
    [(W_CheckWhileCondition M-State statement) (HandleWhile (step-through (W_GetWhileBody statement) M-State) statement)]
    [else M-State]
    )))


;******************************Handle Return Statement********************************************

;R_GetReturn, helper function for HandleReturn, takes in a return statement returns the return value/expression in the return statement
(define R_GetReturn
  (lambda (statement)
    (car (cdr statement))
    ))

;HandleReturn -> returns the return statement in proper form
(define HandleReturn
  (lambda (M-State statement)
    (list (list 'return (M-Value M-State (R_GetReturn statement))))))


;**************************parse tree step through helper functions: ***********************************

;GetFirstStatement - takes in the parsed program returns the first statement of the parsed program
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

;IsIfStatement takes in a single statement, returns true if it is an if statement
(define IsIfStatement
  (lambda (statement)
    (if (eq? (car statement) 'if)
        #t
        #f)))

;IsWhileStatement takes in a single statement, returns true if it is an if statement
(define IsWhileStatement
  (lambda (statement)
    (if (eq? (car statement) 'while)
        #t
        #f)))

;IsReturnStatement takes in a single statement, returns true if it is a return statement
(define IsReturnStatement
  (lambda (statement)
  (if (eq? (car statement) 'return)
        #t
        #f)))

;IsDone -> takes in M-State, returns true if M-State has a return variable
(define IsDone
  (lambda (M-State)
    (not (IsVarUndeclared M-State 'return))
    ))

;takes in the return val, returns the return value in proper form
(define HandleDone
  (lambda (returnVal)
    (cond
      [(eq? #t returnVal) 'true]
      [(eq? #f returnVal) 'false]
      [else returnVal])))

;step-through takes program: the parsed program, M-state: a list of bindings
;it is used to step through each line of the program, it returns the return value if the program returned, or M-State if it didn't
(define step-through
  (lambda (program M-State)
   (step-through-cps program M-State (lambda(v) v))))
    ;(cond
     ; [(IsDone M-State) (HandleDone (LookupValue M-State 'return))];if program returned this ends the program and returns the return value
      ;[(null? program) M-State]; need to check this to prevent errors with checking the cdr or car of an empty list, if program ends unexpectedly, will print M-State
      ;[(IsVarDecStatement (GetFirstStatement program)) (step-through (cdr program) (HandleVarDec M-State (GetFirstStatement program)))]
      ;[(IsAssignStatement (GetFirstStatement program)) (step-through (cdr program) (HandleAssign M-State (GetFirstStatement program)))]
      ;[(IsIfStatement (GetFirstStatement program)) (step-through (cdr program) (HandleIf M-State (GetFirstStatement program)))]
      ;[(IsWhileStatement (GetFirstStatement program)) (step-through (cdr program) (HandleWhile M-State (GetFirstStatement program)))]
      ;[(IsReturnStatement (GetFirstStatement program)) (step-through '() (HandleReturn M-State (GetFirstStatement program)))]; just returns
      ;[else M-State])));if the program ends without a return statement, just print M-State so you can see all the variables


;step-through-cps is the same as step-through but in cps style
(define step-through-cps
  (lambda (program M-State return)
   (cond
     [(IsDone M-State) (return (HandleDone (LookupValue M-State 'return)))];if program returned this ends the program and returns the return value
     [(null? program) (return M-State)]; need to check this to prevent errors with checking the cdr or car of an empty list, if program ends unexpectedly, will print M-State
     [(IsVarDecStatement (GetFirstStatement program)) (step-through-cps (cdr program) (HandleVarDec M-State (GetFirstStatement program)) return)]
     [(IsAssignStatement (GetFirstStatement program)) (step-through-cps (cdr program) (HandleAssign M-State (GetFirstStatement program)) return)]
     [(IsIfStatement (GetFirstStatement program)) (step-through-cps (cdr program) (HandleIf M-State (GetFirstStatement program)) return)]
     [(IsWhileStatement (GetFirstStatement program)) (step-through-cps (cdr program) (HandleWhile M-State (GetFirstStatement program)) return)]
     [(IsReturnStatement (GetFirstStatement program)) (step-through-cps '() (HandleReturn M-State (GetFirstStatement program)) return)]; just returns
     [else (return M-State)])));if the program ends without a return statement, just print M-State so you can see all the variables

;Main Interpreter function

;Accepts a filename ex: (interpret "testProgram.txt")
;Outputs the return statement of the Simple Program

(define interpret
  (lambda (filename)
    (step-through (parser filename) '())))

;Test Cases:
;
;(eq? (interpret "t1.txt") 150)
;(eq? (interpret "t2.txt") -4)
;(eq? (interpret "t3.txt") 10)
;(eq? (interpret "t4.txt") 16)
;(eq? (interpret "t5.txt") 220)
;(eq? (interpret "t6.txt") 5)
;(eq? (interpret "t7.txt") 6)
;(eq? (interpret "t8.txt") 10)
;(eq? (interpret "t9.txt") 5)
;(eq? (interpret "t10.txt") -39)
;(eq? (interpret "t12.txt") ); should give error
;(eq? (interpret "t13.txt") ) ;should give error
;(eq? (interpret "t14.txt") ) ;should give error
;(eq? (interpret "t15.txt") ) ;should give error
;(eq? (interpret "t16.txt") 100)
;(eq? (interpret "t17.txt") 'false)
;(eq? (interpret "t18.txt") 'true)
;(eq? (interpret "t19.txt") 128)
;(eq? (interpret "t20.txt") 12)

;(interpret "testProgram.txt")
