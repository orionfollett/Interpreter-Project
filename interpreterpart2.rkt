#lang racket
(require "simpleParser.rkt")

;Orion Follett
;Prithik Karthikeyan


;Order of functions - Entry point of program is at the bottom!

;main interpret function
;General Helper Functions
;M-Expression and Associated Functions
;M-Value
;M-State
;Variable Declation
;Assignment
;If statements
;While statements
;Code block handling
;Try Catch
;Main step-through loop helper functions
;Commented out tests and personal comments

;Main Interpreter function

;Accepts a filename ex: (interpret "testProgram.txt")
;Outputs the return statement of the Simple Program (or M-State if there is no return statement for debugging purposes)

(define interpret
  (lambda (filename)
     (step-through (parser filename) '())))

;(interpret "t.txt")


;********************************General Helper Functions********************************

;append but cps style
(define append-cps
  (lambda (l1 l2 return)
    (if (null? l1)
        (return l2)
        (append-cps (cdr l1) l2 (lambda(v) (return (cons (car l1) v)))))))

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
      [else (cons (car lis) (replaceall* t r (cdr lis)))])))
      
;takes an atom, returns true if it is an integer operation
(define int-operation?
  (lambda (x)
    (if (or (eq? x '+) (eq? x '-) (eq? x '/) (eq? x '*) (eq? x '%))
        #t
        #f)))

;takes an atom, returns true if it is a bool operation
(define bool-operation?
  (lambda (x)
    (if (or (eq? x '>) (eq? x '<) (eq? x '==) (eq? x '<=) (eq? x '>=) (eq? x '!=) (eq? x '&&) (eq? x '||) (eq? x '!))
        #t
        #f)))

;takes an atom, returns true if it is an integer or a boolean operation
(define operation?
  (lambda (x)
    (if (or (bool-operation? x) (int-operation? x))
        #t
        #f)))

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
     (not (or (operation? x) (eq? x 'true)  (eq? x 'false) (number? x)))))    

;custom-bool-literal? -> takes an atom, returns #t if it is a valid bool literal
(define custom-bool-literal?
  (lambda (x)
    (or (eq? x 'true) (eq? x 'false))))

;bool? -> takes an atom, returns #t if it is either #t or #f, returns false if it isnt
(define bool?
  (lambda (x)
    (cond
      [(or (eq? x #t) (eq? x #f)) #t]
      [else #f])))

;ConvertToSchemeBool - takes val that is either 'true or 'false, returns #t if it is 'true, #f is it is 'false
(define ConvertToSchemeBool
  (lambda (val)
    (cond
      [(eq? val 'true) #t]
      [(eq? val 'false) #f]
      [else (error "Value that is not a bool trying to be converted into a bool!")])))

;ConvertToCustomBool - takes val that is either #t or #f converts it to 'true or 'false
(define ConvertToCustomBool
  (lambda (val)
    (cond
      [(eq? val #t) 'true]
      [(eq? val #f) 'false]
      [else (error "Value that is not a bool trying to be converted into a bool!")])))


;*******************************M_Expression Functions**************************

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
      [else (error "No way to resolve expression!" expression)])))

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
      [(IsVarUndeclared? M-State (car varList)) (error " variable not defined in an expression variable name: " (car varList))]
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
      [(IsVarUndeclared? M-State val) (error "Undeclared variable!" val)] ;undeclared variable
      [else (LookupValue M-State val)]))) ;declared variable that needs to be resolved to a value


;*************************M-State Helper Functions**************************

;M-State format: '((return returnval)(x 0)(y 3)(varname value)...) contains all declared variables
;Updated M-State format -> '(((x 3)) (y 2) (d 3))can have nested bindings

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
(define IsVarUndeclared?
  (lambda (M-State name)
    (cond
      [(null? M-State) #t]
      [(IsNewLayer? M-State) (and (IsVarUndeclared? (GetFirstLayer M-State) name) (IsVarUndeclared? (RemoveLayer M-State) name))]
      [(eq? (GetFirstBindingName M-State) name) #f]
      [else (IsVarUndeclared? (cdr M-State) name)])))

;IsNewLayer? takes in M-State, returns true if it has a new layer on it, false otherwise
(define IsNewLayer?
  (lambda(M-State)
    (cond
      [(null? M-State) #f]
      [(null? (car M-State)) #t]
      [(list? (car (car M-State))) #t]
      [else #f])))

;takes M-State returns M-State with the outermost layer removed
(define RemoveLayer
 (lambda(M-State)
  (cdr M-State)))

;takes M-State that is multilayered, returns first layer
(define GetFirstLayer
  (lambda(M-State)
    (car M-State)))

;AddNewBinding -> takes in M-State, variable name, variable value, returns M-State with new binding
;if M-State has multiple layers, it puts it in the deepest layer because that is the current active layer
(define AddNewBinding
  (lambda (M-State varName varVal)
    (cond
      [(IsNewLayer? M-State) (cons (AddNewBinding (GetFirstLayer M-State) varName varVal) (RemoveLayer M-State))]
      [else (append M-State (list (list varName varVal)))])))

;RemoveBinding -> takes in M-State, variable name returns M-State without that variable
;If the binding doesnt exist, M-State is unchanged
(define RemoveBinding
  (lambda (M-State varName)
    (cond
     [(null? M-State) M-State]
     [(IsNewLayer? M-State) (cons (RemoveBinding (GetFirstLayer M-State) varName) (RemoveBinding (RemoveLayer M-State) varName))]
     [(eq? (GetFirstBindingName M-State) varName) (cdr M-State)]
     [else (cons (GetFirstBinding M-State) (RemoveBinding (cdr M-State) varName))])))

;PopFirstBinding - removes first binding
(define PopFirstBinding
(lambda (M-State)
(cdr M-State)))

;ChangeFirstBindingValue - takes in M-State and a value, changes the value of the first binding and returns the updated M-State
;assumes M-State has no layers and is not empty
(define ChangeFirstBindingValue
  (lambda (M-State varVal)
    (cons (list (GetFirstBindingName M-State) varVal) (PopFirstBinding M-State))))

;ChangeBinding -> takes in M-State, variable name, new variable value, returns M-State with old variable value replaced by new variable value
;If the binding doesnt exist, it creates a new one

(define ChangeBinding
  (lambda (M-State varName varVal)
    (cond
      [(IsVarUndeclared? M-State varName) (AddNewBinding M-State varName varVal)]
      [else (ChangeBinding-Exists M-State varName varVal)])))

;changes the value of a binding, knowing that the binding exists
(define ChangeBinding-Exists
  (lambda (M-State varName varVal)
    (cond
     [(null? M-State) '()]
     [(IsNewLayer? M-State) (cons (ChangeBinding-Exists (GetFirstLayer M-State) varName varVal) (ChangeBinding-Exists (RemoveLayer M-State) varName varVal))]
     [(eq? (GetFirstBindingName M-State) varName) (ChangeFirstBindingValue M-State varVal)]
     [else (cons (GetFirstBinding M-State) (ChangeBinding-Exists (cdr M-State) varName varVal))])))

;takes in two atoms, returns null if they are both null, or the value of the one that is not null
(define ResolveMultiLayerSearch
  (lambda(a1 a2)
    (cond
      [(and (eq? a1 'null) (eq? a2 'null)) 'null]
      [(eq? a1 'null) a2]
      [else a1])))

;LookupValue -> takes in M-State, variable name, returns the value associated with that variable
;returns 'null if there is no value associated with that variable
(define LookupValue
  (lambda (M-State varName)
    (cond
      [(null? M-State) 'null]
      [(IsNewLayer? M-State) (ResolveMultiLayerSearch (LookupValue (GetFirstLayer M-State) varName) (LookupValue (RemoveLayer M-State) varName))]
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
      [(IsVarUndeclared? M-State (VD_GetVarName statement)) (AddNewBinding M-State (VD_GetVarName statement) (M-Value M-State (VD_GetVarValue statement)))]
      [else (error "Error: " (VD_GetVarName statement) "variable already declared")])))
    

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
      [(IsVarUndeclared? M-State (AS_GetVarName statement)) (error (AS_GetVarName statement) "Assignment before declaration!")]
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
    (list (car (cdr (cdr statement))))))

;I_GetNext takes in if statement returns next if condition (should return either the else body or an empty list if there isnt one)
(define I_GetNext
  (lambda (statement)
    (cond
      [(null? statement) '()]
      [(null? (cdr (cdr (cdr statement)))) '()]
      [else (list (car (cdr (cdr (cdr statement)))))])))

;I_IsIf? takes a statement, returns true if the beginning is a full if block
(define I_IsIf?
  (lambda (statement)
    (eq? (car statement) 'if)))

;HandleIf -> Takes in M-State and an if statement, returns updated M-State
(define HandleIf
  (lambda (M-State statement return break continue throw)
    (cond
    [(null? statement) M-State];none of the if statements were true
    [(M-Value M-State (I_GetIfCondition statement)) (step-through-cc (I_GetIfBody statement) M-State return break continue throw)] ;if statement was true, so run the body
    [(and (not (null? (I_GetNext statement))) (I_IsIf? (I_GetNext statement))) (HandleIf M-State (I_GetNext statement) return break continue throw)] ;if statement was false, but there are more ifs to check check the next one
    [(null? (I_GetNext statement)) M-State];nothing left to check, return the state
    [else (step-through-cc (I_GetNext statement) M-State return break continue throw)])));there is an else statement remaining, run that code, then return the updated M-State


;*****************************While Statement functions ******************************************

;W_ indicates it is a helper function for HandleWhile so it should only be used for that
;W_GetWhileCondition takes in a while statement and returns the loop condition
(define W_GetWhileCondition
  (lambda (statement)
    (car (cdr statement))))

;W_CheckWhileCondition takes in a while statement and return true or false depending on if it is true or false
(define W_CheckWhileCondition
  (lambda (M-State statement)
    (M-Value M-State (W_GetWhileCondition statement))))

;W_GetWhileBody takes in a while statement and returns the body of the loop
(define W_GetWhileBody
  (lambda (statement)
    (list (car (cdr (cdr statement))))))

;main while loop function
(define loop
  (lambda(M-State statement return break continue throw)
   (cond
    [(W_CheckWhileCondition M-State statement) 
                          (loop (call/cc (lambda (c) (step-through-cc (W_GetWhileBody statement) M-State return break c throw))) statement return break continue throw)]
    [else M-State])));loop condition is no longer true, loop is done

;HandleWhile -> Takes in M-State and a while statement and a break continuation, returns updated M-State
(define HandleWhile
 (lambda (M-State statement return break continue throw)
   (loop M-State statement return break continue throw)))


;******************************Handle Return Statement********************************************

;R_GetReturn, helper function for HandleReturn, takes in a return statement returns the return value/expression in the return statement
(define R_GetReturn
  (lambda (statement)
    (car (cdr statement))))

;HandleReturn -> returns the return statement in proper form
(define HandleReturn
  (lambda (M-State statement return)
    ;(list (list 'return (M-Value M-State (R_GetReturn statement))))))
    (return (M-Value M-State (R_GetReturn statement)))))


;****************************************Handle Code Block**********************************************

;helper function for HandleCodeBlock, adds a new layer to M-State
(define CB_AddLayer
  (lambda (M-State)
   (cons '() M-State)))

;helper function for RemoveLayer, removes the last added layer of M-State
(define CB_RemoveLayer
(lambda(M-State)
  (cdr M-State)))

;helper function for HandleCodeBlock, returns the body of the code without the begin statement
(define CB_GetBody
  (lambda(statement)
    (cdr statement)))

;takes a code block beginning with begin statement, returns M-State after resolving that code block
(define HandleCodeBlock
  (lambda(M-State statement return break continue throw)
     (CB_RemoveLayer (step-through-cc (CB_GetBody statement) (CB_AddLayer M-State) return break continue throw))))


;**************************************Throw**************************************

;gets expression part of statement after throw keyword
(define T_GetBody
  (lambda(statement)
    (car (cdr statement))))

;calls throw continuation sending current M-State and thrown value packaged together 
(define HandleThrow
  (lambda(M-State statement throw)
    (throw (list (M-Value M-State (T_GetBody statement)) M-State))))

 ;global helper function to get thrown value from throw continuation return
 (define Throw_GetValue
   (lambda(lis)
   (car lis)))
  
  ;global helper function to get M-State from throw continuation return
 (define Throw_GetMState
   (lambda(lis)
   (car (cdr lis))))

  
;**************************************TryCatch**************************************

;returns #t if there is a catch body, false otherwise
(define TC_IsCatchBody?
  (lambda(statement)
    (not (null? (car (cdr (cdr statement)))))))

;returns #t if there is a finally body, false otherwise
(define TC_IsFinallyBody?
  (lambda(statement)
    (not (null? (car (cdr (cdr (cdr statement))))))))

;returns the  body of the try statement with the "try" keyword stripped
(define TC_GetTryBody
  (lambda(statement)
    (car (cdr statement))))

;returns the catch body with the catch keyword and variable name stripped
(define TC_GetCatchBody
  (lambda(statement)
    (cond
      [(TC_IsCatchBody? statement) (car (cdr (cdr (car (cdr (cdr statement))))))]
      [else '()])))

;returns finally body with finally word stripped
(define TC_GetFinallyBody
  (lambda(statement)
    (cond
    [(TC_IsFinallyBody? statement) (car (cdr (car (cdr (cdr (cdr statement))))))]
    [else '()])))

;gets name of catch variable so it can be referenced in catch block
(define TC_GetVarName
  (lambda(statement)
    (cond
      [(TC_IsCatchBody? statement) (car (car (cdr (car (cdr (cdr statement))))))];if there is no catch body, no catch variable to add
      [else 'null])))

;returns M-State with a binding for the catch variable
(define TC_AddCatchValueToMState
  (lambda(M-State varName thrown_value)
    (cond
      [(IsVarUndeclared? M-State varName) (AddNewBinding M-State varName thrown_value)]
      [else (error "error name in catch already been used")])))

;analyzes thrown_value to see if it is M-State alone, or thrown-value packaged with M-State, returns true if there was athrow intry, false otherwise
(define TC_GotThrown?
  (lambda(tv)
    (cond
      [(null? tv) #f]
      [else (not (list? (car tv)))])))

;handles the catch block and returns M-State after completion
(define TC_HandleCatch
  (lambda(varName thrown_value body return break continue throw)
    (cond;check if thrown value is number or mstate
      [(or (eq? varName 'null) (not (TC_GotThrown? thrown_value))) thrown_value];if it didnt get thrown return M-State, if itdid get thrown,, run catch block
      ;thrown value is a literal, run catch block
      [else (RemoveBinding (step-through-cc body (TC_AddCatchValueToMState (Throw_GetMState thrown_value) varName (Throw_GetValue thrown_value)) return break continue throw) varName)])))

;handles a generic block, either try block or finally block
(define TC_HandleGeneric
  (lambda(M-State body return break continue throw)
    (step-through-cc body M-State return break continue throw)))

;takes in a try catch statement, continuations, and M-State, updates M-State based on the statement
(define HandleTryCatch
  (lambda(M-State statement return break continue throw)
    ;run try block, return value of try block run into catch block, catch sees if it should run itself, then run finally block
    (TC_HandleGeneric
     (TC_HandleCatch (TC_GetVarName statement) (call/cc (lambda(throw)
      (TC_HandleGeneric M-State (TC_GetTryBody statement) return break continue throw)))
       (TC_GetCatchBody statement) return break continue throw)
        (TC_GetFinallyBody statement) return break continue throw)))


;**************************parse tree step through helper functions: ***********************************

;GetFirstStatement - takes in the parsed program returns the first statement of the parsed program
(define GetFirstStatement
  (lambda (program)
    (if (list? (car program))
     [car program]
     program)))

;IsVarDecStatement takes in a single statement, returns true or false depending on if it is a variable decalaration or not
(define IsVarDecStatement?
  (lambda (statement)
    (cond
      [(null? statement) #f]
      [(eq? (car statement) 'var) #t]
      [else #f])))

;IsAssignStatement takes in a single statement, returns true or false depending on if it is an assignment
(define IsAssignStatement?
  (lambda (statement)
    (cond
      [(null? statement) #f]
      [(eq? (car statement) '=) #t]
      [else #f])))

;IsIfStatement takes in a single statement, returns true if it is an if statement
(define IsIfStatement?
  (lambda (statement)
    (cond
      [(null? statement) #f]
      [(eq? (car statement) 'if) #t]
      [else #f])))

;IsWhileStatement takes in a single statement, returns true if it is an if statement
(define IsWhileStatement?
  (lambda (statement)
    (cond
      [(null? statement) #f]
      [(eq? (car statement) 'while) #t]
      [else #f])))

;IsReturnStatement takes in a single statement, returns true if it is a return statement
(define IsReturnStatement?
  (lambda (statement)
    (cond
      [(null? statement) #f]
      [(eq? (car statement) 'return) #t]
      [else #f])))

;IsCodeBlock takes in a single statement, returns true if it is a begin statement
(define IsCodeBlockStatement?
  (lambda (statement)
    (cond
      [(null? statement) #f]
      [(eq? (car statement) 'begin) #t]
      [else #f])))

;IsBreakStatement? takes in a single statement, returns true if it is a break statement
(define IsBreakStatement?
  (lambda (statement)
    (cond
      [(null? statement) #f]
      [(eq? (car statement) 'break) #t]
      [else #f])))

;IsContinueStatement? takes in a single statement, returns true if it is a continue statement
(define IsContinueStatement?
  (lambda (statement)
    (cond
      [(null? statement) #f]
      [(eq? (car statement) 'continue) #t]
      [else #f])))

;IsThrowStatement? takes in a single statement, returns true if it is a throw statement
(define IsThrowStatement?
  (lambda (statement)
    (cond
      [(null? statement) #f]
      [(eq? (car statement) 'throw) #t]
      [else #f])))

;IsTryCatchStatement? takes in a single statement, returns true if it is a try catch statement
(define IsTryCatchStatement?
  (lambda (statement)
    (cond
      [(null? statement) #f]
      [(eq? (car statement) 'try) #t]
      [else #f])))

;ReturnProgram - takes in the result of the program, formats what to output at the end
(define FormatReturn
   (lambda (returnVal)
    (cond
      [(eq? #t returnVal) 'true]
      [(eq? #f returnVal) 'false]
      [else returnVal])))

;************************************Default Continutations*******************************

(define STD_BREAK
  (lambda(v) (error "Break outside of loop")))

(define STD_CONT
  (lambda(v) (error "Continue outside of loop")))

(define STD_THROW
  (lambda(v) (error "Error: " v)))

;**************************************MAIN STEP THROUGH LOOP******************************************

;step-through takes program: the parsed program, M-state: a list of bindings
;it is used to step through each line of the program, it returns the return value if the program returned, or M-State if it didn't
;this should only be run at start of program
(define step-through
  (lambda (program M-State)
   (FormatReturn (call/cc (lambda (return) (step-through-cc program M-State return STD_BREAK STD_CONT STD_THROW))))))

;step-through-cps is the call-cc helper function for step-through so that return continuation can operate correctly
(define step-through-cc
  (lambda (program M-State return break continue throw)
   (cond
     ; need to check this first to prevent errors with checking the cdr or car of an empty list, if program ends unexpectedly, will print M-State
     [(null? program) M-State]
     [(IsVarDecStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (HandleVarDec M-State (GetFirstStatement program)) return break continue throw)]
     [(IsAssignStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (HandleAssign M-State (GetFirstStatement program)) return break continue throw)]
     [(IsIfStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (HandleIf M-State (GetFirstStatement program) return break continue throw) return break continue throw)]
     [(IsWhileStatement? (GetFirstStatement program))
      (step-through-cc (cdr program) (call/cc (lambda(break)
                                      (HandleWhile M-State (GetFirstStatement program) return break continue throw))) return break continue throw)]
     [(IsTryCatchStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (HandleTryCatch M-State (GetFirstStatement program) return break continue throw) return break continue throw)]
     [(IsCodeBlockStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (HandleCodeBlock M-State (GetFirstStatement program) return break continue throw) return break continue throw)]
     [(IsBreakStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (break (RemoveLayer M-State)) return STD_BREAK continue throw)] ;breaks out of a loop or errors if no loop
     [(IsContinueStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (continue M-State) return break STD_CONT throw)] ;goes back to beginning of loop or errors if no loop
     [(IsReturnStatement? (GetFirstStatement program)) (HandleReturn M-State (GetFirstStatement program) return)]; just returns M-State with only return value
     [(IsThrowStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (HandleThrow M-State (GetFirstStatement program) throw) return break continue STD_THROW)] 
     [else M-State])));if the program ends without a return statement, just print M-State so you can see all the variables

;Test Cases:
;
(list '1 (eq? (interpret "t1.txt") 150))
(list '2 (eq? (interpret "t2.txt") -4))
(list '3 (eq? (interpret "t3.txt") 10))
(list '4 (eq? (interpret "t4.txt") 16))
(list '5 (eq? (interpret "t5.txt") 220))
(list '6 (eq? (interpret "t6.txt") 5))
(list '7 (eq? (interpret "t7.txt") 6))
(list '8 (eq? (interpret "t8.txt") 10))
(list '9 (eq? (interpret "t9.txt") 5))
(list '10 (eq? (interpret "t10.txt") -39))
;(eq? (interpret "t12.txt") ); should give error
;(eq? (interpret "t13.txt") ) ;should give error
;(eq? (interpret "t14.txt") ) ;should give error
;(eq? (interpret "t15.txt") ) ;should give error
(list '12 (eq? (interpret "t16.txt") 100))
(list '13 (eq? (interpret "t17.txt") 'false))
(list '14 (eq? (interpret "t18.txt") 'true))
(list '15 (eq? (interpret "t19.txt") 128))
(list '16 (eq? (interpret "t20.txt") 12))
(list '17 (eq? (interpret "t21.txt") 20))
(list '18 (eq? (interpret "t22.txt") 164))
(list '19 (eq? (interpret "t23.txt") 32))
(list '20 (eq? (interpret "t24.txt") 2))
;(eq? (interpret "t25.txt") ) ;should give error
(list '21 (eq? (interpret "t26.txt") 25))
(list '22 (eq? (interpret "t27.txt") 21))
(list '23 (eq? (interpret "t28.txt") 6))
(list '24 (eq? (interpret "t29.txt") -1))
(list '25 (eq? (interpret "t30.txt") 789))
;(eq? (interpret "t31.txt") ) ; should return error
;(eq? (interpret "t32.txt") ) ; should return error
;(eq? (interpret "t33.txt") ) ; should return error
;(list '26 (eq? (interpret "t34.txt") 12 )) ;
(list '27 (eq? (interpret "t35.txt") 125))
(list '28 (eq? (interpret "t36.txt") 110))
(list '29 (eq? (interpret "t37.txt") 2000400)) ; not working
(list '30 (eq? (interpret "t38.txt") 101))
;(eq? (interpret "t39.txt")) ; should return error
(list '31 (eq? (interpret "t40.txt") 9)) ; 
(list '32 (eq? (interpret "t41.txt") 5)) ;


;Part 2 General Idea/ List of Features
;2. break, continue, throw
;3. try catch finally 
;
;For scoping, M-state will now be a more nested list, '(((f 5) (h 6)) (d 4) (y 2) (x 1))
;when a code block begins, start a new layer in M-State by consing an empty list onto M-State, '(() (x 3) (r 4) (e 5))
;when a code block ends, pop off layer that corresponds to that code block by removing the car of Mstate
;this means that the order of M-State will matter
;task 1 is to modify M-state functions to account for these changes.
;Takse  1.1 modify add and remove binding to work for multilayered things
;task 2 is write handle code block to add a layer at the beginning and remove a layer at the end

;Part 1 General Idea:
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





