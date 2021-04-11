#lang racket
;(require "simpleParser.rkt")
(require "functionParser.rkt")

;Orion Follett
;Prithik Karthikeyan

;Main Interpreter function

;Accepts a filename ex: (interpret "testProgram.txt")
;Outputs the return statement of the Simple Program (or env if there is no return statement for debugging purposes)

(define interpret
  (lambda (filename)
     (step-through (parser filename) '())))

;Plan of Action for Functions:

;1. change large scale things, only variable and function declaration and assignment statements outside of functions, analyze everything then run main
;2. change env and all that to work for global variable, function definitions and to set up environments
;3. add function calls and make sure they work with global and local variables, the flow will be: set up environment and map formal parameter, run the function, return whatever is returned



;notes on how to make functions:
; functions by default will not return anything, but a return continuation will just be called if it happens to return something
;in env, functions needs to have the following information: name of function and closure, (where closure is formal parameters, body of function, and a function to make the environment)

;**************************************MAIN STEP THROUGH LOOP******************************************

;step-through takes program: the parsed program, env: a list of bindings
;it is used to step through each line of the program, it returns the return value if the program returned, or env if it didn't
;this should only be run at start of program
(define step-through
  (lambda (program env)
   (FormatReturn (call/cc (lambda (return) (step-through-outer-layer program env return STD_BREAK STD_CONT STD_THROW))))))

;step-through-outer-layer - collects all the global variables and function declarations outside of main
(define step-through-outer-layer
  (lambda (program env return break continue throw)
    (cond
      [(null? program) (step-through-cc (GetMain env) (CB_AddLayer env) return break continue throw)];add layer since you are entering a function
      [(IsVarDecStatement? (GetFirstStatement program)) (step-through-outer-layer (cdr program) (HandleVarDec env (GetFirstStatement program) throw) return break continue throw)]
      [(IsAssignStatement? (GetFirstStatement program)) (step-through-outer-layer (cdr program) (HandleAssign env (GetFirstStatement program) throw) return break continue throw)]
      [(IsFuncDecStatement? (GetFirstStatement program)) (step-through-outer-layer (cdr program) (HandleFuncDec env (GetFirstStatement program) return break continue throw) return break continue throw)]
      [else (error "Statement not allowed outside of function or undefined")]
      )))

;step-through-cps is the call-cc helper function for step-through so that return continuation can operate correctly
(define step-through-cc
  (lambda (program env return break continue throw)
   (cond
     ; need to check this first to prevent errors with checking the cdr or car of an empty list, if program ends unexpectedly, will print env
     [(null? program) env]
     [(IsVarDecStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (HandleVarDec env (GetFirstStatement program) throw) return break continue throw)]
     [(IsAssignStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (HandleAssign env (GetFirstStatement program) throw) return break continue throw)]
     [(IsIfStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (HandleIf env (GetFirstStatement program) return break continue throw) return break continue throw)]
     [(IsWhileStatement? (GetFirstStatement program))
      (step-through-cc (cdr program) (call/cc (lambda(break)
                                      (HandleWhile env (GetFirstStatement program) return break continue throw))) return break continue throw)]
     [(IsTryCatchStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (HandleTryCatch env (GetFirstStatement program) return break continue throw) return break continue throw)]
     [(IsCodeBlockStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (HandleCodeBlock env (GetFirstStatement program) return break continue throw) return break continue throw)]
     [(IsBreakStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (break (RemoveLayer env)) return STD_BREAK continue throw)] ;breaks out of a loop or errors if no loop
     [(IsContinueStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (continue env) return break STD_CONT throw)] ;goes back to beginning of loop or errors if no loop
     [(IsReturnStatement? (GetFirstStatement program)) (HandleReturn env (GetFirstStatement program) return)]
     [(IsThrowStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (HandleThrow env (GetFirstStatement program) throw) return break continue STD_THROW)]
     [(IsFuncDecStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (HandleFuncDec env (GetFirstStatement program) return break continue throw) return break continue throw)]
     [(IsFuncallStatement? (GetFirstStatement program)) (step-through-cc (cdr program) (HandleFuncall env (GetFirstStatement program) return break continue throw) return break continue throw)]
     [else env])));if the program ends without a return statement, just print env so you can see all the variables


;****************************************Handle Func Definition****************************************

;places the function definition in the current environment
;function definition is of the form: (funcname closure)
;where closure is ((list of formal parameters) (body) (env))
(define HandleFuncDec
  (lambda(env statement return break continue throw)
    (AddFunctionDecToEnv env (FD_GetFuncName statement) (FD_GetFuncParam statement) (FD_GetFuncBody statement))
    ))

(define FD_GetFuncName
  (lambda (statement)
    (car (cdr statement))))

(define FD_GetFuncParam
  (lambda (statement)
    (car (cdr (cdr statement)))))

(define FD_GetFuncBody
  (lambda (statement)
    (car (cdr (cdr (cdr statement))))))

;****************************************Handle Function Call*******************************************

;returns main body
(define GetMain
  (lambda(env)
    (if (IsVarUndeclared? env 'main)
        (error "no main function")
        (FC_GetFuncBody env 'main))))
    
(define FC_GetFuncName
  (lambda (statement)
    (car (cdr statement))))

(define FC_GetFormalParams
  (lambda(env name)
    (car (LookupValue env name))))

;collects all the args passed to function
(define FC_GetArgs
  (lambda(statement)
    (cdr (cdr statement))))

(define FC_ResolveArgs
  (lambda(env args throw)
    (cond
      [(null? args) args]
      [else (cons (M-Value env (car args) throw) (FC_ResolveArgs env (cdr args) throw))]
    )))
      
(define FC_GetFuncBody
  (lambda (env name)
    (car (cdr (LookupValue env name)))))

;removes current layer, keeps global variables and function definitions adds formal params to neew layer in env and sets their value to be the arguments passed into the function
(define FC_PrepEnv
  (lambda(env fparams args throw);fparams are formal parameters that come from function definition, args are arguments being passed into function, they must already be resolved to values
    (cond
      [(and (null? args) (null? fparams)) env]
      [(or (null? args) (null? fparams)) (error "Mismatch number of arguments in function call")]
      [else (FC_PrepEnv (AddNewBinding env (car fparams) (M-Value env (car args) throw)) (cdr fparams) (cdr args) throw)]
     )))

;calls FC_UpdateEnv with correct paramters (because there couldve been a return value)
;return format (return val env)
(define FC_HandleFuncOutput
  (lambda(env output)
    (cond
      [(null? output) env]
      [(eq? (car output) 'return) (FC_UpdateEnv env (cdr(car (cdr (cdr output)))))]
      [else  (FC_UpdateEnv env output)]
    )))

;'(() (x 1) (test ((p1 p2) ((return 1)))) (main (() ((var y (funcall test 1 2)) (return y)))))
;'(((p1 1) (p2 2)) (x 1) (test ((p1 p2) ((return 1)))) (main (() ((var y (funcall test 1 2)) (return y)))))

;'(() (x 1) (test ((p1 p2) ((var t 1) (return 1)))) (main (() ((var y (funcall test 1 2)) (return y)))))
;'(((p1 1) (p2 2) (t 1)) (x 1) (test ((p1 p2) ((var t 1) (return 1)))) (main (() ((var y (funcall test 1 2)) (return y)))))

;(HandleFuncall '(() (x 1) (test ((p1 p2) ((var t 1) (return 1)))) (main (() ((var y (funcall test 1 2)) (return y)))))
 ;             '(funcall test 1 2) #f #f #f #f)

;adjusts global variables that changed, loops through changed and calls changebinding on env
(define FC_UpdateEnv
  (lambda(env changed)
    ;(list env '() changed)
    (cond
      [(or (null? env) (null? changed)) env]
      [(IsNewLayer? changed) (cons (FC_UpdateEnv env (car changed)) (FC_UpdateEnv env (cdr changed)))]
      [else (ChangeBinding-Exists env (GetFirstBindingName changed) (GetFirstBindingValue changed))])))

;HandleNormalCall returns an updated env based on function call
(define FC_HandleNormalCall
  (lambda (env statement return break continue throw)
     (call/cc (lambda (return-from-function)
      (CB_RemoveLayer (step-through-cc (FC_GetFuncBody env (FC_GetFuncName statement)) ;get function body
       (FC_PrepEnv (CB_AddLayer (CB_RemoveLayer env)) (FC_GetFormalParams env (FC_GetFuncName statement)) (FC_ResolveArgs env (FC_GetArgs statement) throw) throw) ; prepare env for function
        return-from-function STD_BREAK STD_CONT throw))))));pass in continuations

;HandleFuncall either returns an updated env or the return value depending on the return continuation
(define HandleFuncall
  (lambda (env statement return break continue throw)
    (if (eq? return 'M-F)
        (FC_FormatMF (FC_HandleMFunctionCall env statement return break continue throw)) ; update env but return just a value
        (FC_HandleFuncOutput env (FC_HandleNormalCall env statement return break continue throw))))) ; update env if global variables changes

;format func call if it is being used for m value, if it returns an env then send null so that it can error
(define FC_FormatMF
  (lambda (output)
    (if (list? output)
        'null
        output
    )))

;HandleMFunctionCall returns the return value of the function and changes global variables that could have changed
(define FC_HandleMFunctionCall
  (lambda (env statement return break continue throw)
     (call/cc (lambda (return-from-function) (CB_RemoveLayer (step-through-cc (FC_GetFuncBody env (FC_GetFuncName statement)) ;get function body
      (FC_PrepEnv (CB_AddLayer (CB_RemoveLayer env)) (FC_GetFormalParams env (FC_GetFuncName statement)) (FC_ResolveArgs env (FC_GetArgs statement) throw) throw) ; prepare env for function
       return-from-function STD_BREAK STD_CONT throw))))));pass in continuations

;****************************************Handle Code Block**********************************************

;helper function for HandleCodeBlock, adds a new layer to env
(define CB_AddLayer
  (lambda (env)
   (cons '() env)))

;helper function for RemoveLayer, removes the last added layer of env
(define CB_RemoveLayer
(lambda(env)
  (cdr env)))

;helper function for HandleCodeBlock, returns the body of the code without the begin statement
(define CB_GetBody
  (lambda(statement)
    (cdr statement)))

;takes a code block beginning with begin statement, returns env after resolving that code block
(define HandleCodeBlock
  (lambda(env statement return break continue throw)
     (CB_RemoveLayer (step-through-cc (CB_GetBody statement) (CB_AddLayer env) return break continue throw))))


;**************************************Throw**************************************

;gets expression part of statement after throw keyword
(define T_GetBody
  (lambda(statement)
    (car (cdr statement))))

;calls throw continuation sending current env and thrown value packaged together 
(define HandleThrow
  (lambda(env statement throw)
    (throw (list (M-Value env (T_GetBody statement) throw) env))))

 ;global helper function to get thrown value from throw continuation return
 (define Throw_GetValue
   (lambda(lis)
   (car lis)))
  
  ;global helper function to get env from throw continuation return
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

;returns env with a binding for the catch variable
(define TC_AddCatchValueToMState
  (lambda(env varName thrown_value)
    (cond
      [(IsVarUndeclared? env varName) (AddNewBinding env varName thrown_value)]
      [else (error "error name in catch already been used")])))

;analyzes thrown_value to see if it is env alone, or thrown-value packaged with env, returns true if there was athrow intry, false otherwise
(define TC_GotThrown?
  (lambda(tv)
    (cond
      [(null? tv) #f]
      [else (not (list? (car tv)))])))

;handles the catch block and returns env after completion
(define TC_HandleCatch
  (lambda(varName thrown_value body return break continue throw)
    (cond;check if thrown value is number or mstate
      [(or (eq? varName 'null) (not (TC_GotThrown? thrown_value))) thrown_value];if it didnt get thrown return env, if itdid get thrown,, run catch block
      ;thrown value is a literal, run catch block
      [else (RemoveBinding (step-through-cc body (TC_AddCatchValueToMState (Throw_GetMState thrown_value) varName (Throw_GetValue thrown_value)) return break continue throw) varName)])))

;handles a generic block, either try block or finally block
(define TC_HandleGeneric
  (lambda(env body return break continue throw)
    (step-through-cc body env return break continue throw)))

;takes in a try catch statement, continuations, and env, updates env based on the statement
(define HandleTryCatch
  (lambda(env statement return break continue throw)
    ;run try block, return value of try block run into catch block, catch sees if it should run itself, then run finally block
    (TC_HandleGeneric
     (TC_HandleCatch (TC_GetVarName statement) (call/cc (lambda(throw)
      (TC_HandleGeneric env (TC_GetTryBody statement) return break continue throw)))
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

;IsFuncDecStatement? takes in a single statemnet, return true if it is a function declaration
(define IsFuncDecStatement?
  (lambda (statement)
    (cond
      [(null? statement) #f]
      [(eq? (car statement) 'function) #t]
      [else #f])))

;IsFuncCallStatement? takes in a single statemnet, return true if it is a function declaration
(define IsFuncallStatement?
  (lambda (statement)
    (cond
      [(null? statement) #f]
      [(eq? (car statement) 'funcall) #t]
      [else #f])))


;ReturnProgram - takes in the result of the program, formats what to output at the end
(define FormatReturn
   (lambda (returnVal)
    (cond
      [(list? returnVal) (FormatReturn (car (cdr returnVal)))]
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

;HandleIf -> Takes in env and an if statement, returns updated env
(define HandleIf
  (lambda (env statement return break continue throw)
    (cond
    [(null? statement) env];none of the if statements were true
    [(M-Value env (I_GetIfCondition statement) throw) (step-through-cc (I_GetIfBody statement) env return break continue throw)] ;if statement was true, so run the body
    [(and (not (null? (I_GetNext statement))) (I_IsIf? (I_GetNext statement))) (HandleIf env (I_GetNext statement) return break continue throw)] ;if statement was false, but there are more ifs to check check the next one
    [(null? (I_GetNext statement)) env];nothing left to check, return the state
    [else (step-through-cc (I_GetNext statement) env return break continue throw)])));there is an else statement remaining, run that code, then return the updated env

;*****************************While Statement functions ******************************************
;W_ indicates it is a helper function for HandleWhile so it should only be used for that
;W_GetWhileCondition takes in a while statement and returns the loop condition
(define W_GetWhileCondition
  (lambda (statement)
    (car (cdr statement))))

;W_CheckWhileCondition takes in a while statement and return true or false depending on if it is true or false
(define W_CheckWhileCondition
  (lambda (env statement throw)
    (M-Value env (W_GetWhileCondition statement) throw)))

;W_GetWhileBody takes in a while statement and returns the body of the loop
(define W_GetWhileBody
  (lambda (statement)
    (list (car (cdr (cdr statement))))))

;main while loop function
(define loop
  (lambda(env statement return break continue throw)
   (cond
    [(W_CheckWhileCondition env statement throw) 
                          (loop (call/cc (lambda (c) (step-through-cc (W_GetWhileBody statement) env return break c throw))) statement return break continue throw)]
    [else env])));loop condition is no longer true, loop is done

;HandleWhile -> Takes in env and a while statement and a break continuation, returns updated env
(define HandleWhile
 (lambda (env statement return break continue throw)
   (loop env statement return break continue throw)))


;******************************Handle Return Statement********************************************
;R_GetReturn, helper function for HandleReturn, takes in a return statement returns the return value/expression in the return statement
(define R_GetReturn
  (lambda (statement)
    (car (cdr statement))))

;HandleReturn -> returns the return statement in proper form:  (return returnval env)
(define HandleReturn
  (lambda (env statement return)
    ;(list (list 'return (M-Value env (R_GetReturn statement))))))
    (return (list 'return (M-Value env (R_GetReturn statement) STD_THROW) env))))

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

;HandleVarDec takes in env and variable declaration statement, returns updated m state
(define HandleVarDec
  (lambda (env statement throw)
    (cond
      [(IsVarUndeclared? env (VD_GetVarName statement)) (AddNewBinding env (VD_GetVarName statement) (M-Value env (VD_GetVarValue statement) throw))]
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
  
;HandleAssign -> Takes in env and an assignment statement, returns updated env
(define HandleAssign
  (lambda (env statement throw)
    (cond
      [(IsVarUndeclared? env (AS_GetVarName statement)) (error (AS_GetVarName statement) "Assignment before declaration!")]
      [else (ChangeBinding env (AS_GetVarName statement) (M-Value env (AS_GetVarVal statement) throw))])))


;*************************env Helper Functions**************************

;env format: '((return returnval)(x 0)(y 3)(varname value)...) contains all declared variables
;Updated env format -> '(((x 3)) (y 2) (d 3))can have nested bindings

;GetFirstBinding -> takes in env, returns the first binding
(define GetFirstBinding
  (lambda (env)
    (cond
      [(null? env) '()]
      [else (car env)])))

;GetFirstBindingName -> takes in env, returns first variable name of first binding
(define GetFirstBindingName
  (lambda (env)
    (cond
     [(null? (GetFirstBinding env)) '()]
     [else (car (GetFirstBinding env))])))

;GetFirstBindingValue -> takes in env, returns first variable value of first binding
(define GetFirstBindingValue
  (lambda (env)
    (cond
      [(null? env) '()]
      [else (unbox (car (cdr (GetFirstBinding env))))])))

;IsNameUnused -> takes in env and variable name, makes sure name is not in the list
(define IsVarUndeclared?
  (lambda (env name)
    (cond
      [(null? env) #t]
      [(IsNewLayer? env) (and (IsVarUndeclared? (GetFirstLayer env) name) (IsVarUndeclared? (RemoveLayer env) name))]
      [(eq? (GetFirstBindingName env) name) #f]
      [else (IsVarUndeclared? (cdr env) name)])))

;IsNewLayer? takes in env, returns true if it has a new layer on it, false otherwise
(define IsNewLayer?
  (lambda(env)
    (cond
      [(null? env) #f]
      [(null? (car env)) #t]
      [(list? (car (car env))) #t]
      [else #f])))

;takes env returns env with the outermost layer removed
(define RemoveLayer
 (lambda(env)
  (cdr env)))

;takes env that is multilayered, returns first layer
(define GetFirstLayer
  (lambda(env)
    (car env)))

;AddNewBinding -> takes in env, variable name, variable value, returns env with new binding
;if env has multiple layers, it puts it in the deepest layer because that is the current active layer
(define AddNewBinding
  (lambda (env varName varVal)
    (cond
      [(IsNewLayer? env) (cons (AddNewBinding (GetFirstLayer env) varName varVal) (RemoveLayer env))]
      [else (append env (list (list varName (box varVal))))])))

;RemoveBinding -> takes in env, variable name returns env without that variable
;If the binding doesnt exist, env is unchanged
(define RemoveBinding
  (lambda (env varName)
    (cond
     [(null? env) env]
     [(IsNewLayer? env) (cons (RemoveBinding (GetFirstLayer env) varName) (RemoveBinding (RemoveLayer env) varName))]
     [(eq? (GetFirstBindingName env) varName) (cdr env)]
     [else (cons (GetFirstBinding env) (RemoveBinding (cdr env) varName))])))

;PopFirstBinding - removes first binding
(define PopFirstBinding
(lambda (env)
(cdr env)))

;ChangeFirstBindingValue - takes in env and a value, changes the value of the first binding and returns the updated env
;assumes env has no layers and is not empty
(define ChangeFirstBindingValue
  (lambda (env varVal)
    ;(cons (list (GetFirstBindingName env) varVal) (PopFirstBinding env))))
    (begin (set-box! (GetFirstBindingValueBox env) varVal) env)))

;GetFirstBindingValueBox -> takes in env, returns first variable value box of first binding
(define GetFirstBindingValueBox
  (lambda (env)
    (cond
      [(null? env) '()]
      [else (car (cdr (GetFirstBinding env)))])))
    
;ChangeBinding -> takes in env, variable name, new variable value, returns env with old variable value replaced by new variable value
;If the binding doesnt exist, it creates a new one

(define ChangeBinding
  (lambda (env varName varVal)
    (cond
      [(IsVarUndeclared? env varName) (AddNewBinding env varName varVal)]
      [else (ChangeBinding-Exists env varName varVal)])))

;changes the value of a binding, knowing that the binding exists
(define ChangeBinding-Exists
  (lambda (env varName varVal)
    (cond
     [(null? env) '()]
     [(IsNewLayer? env) (cons (ChangeBinding-Exists (GetFirstLayer env) varName varVal) (ChangeBinding-Exists (RemoveLayer env) varName varVal))]
     [(eq? (GetFirstBindingName env) varName) (ChangeFirstBindingValue env varVal)]
     [else (cons (GetFirstBinding env) (ChangeBinding-Exists (cdr env) varName varVal))])))

;takes in two atoms, returns null if they are both null, or the value of the one that is not null
(define ResolveMultiLayerSearch
  (lambda(a1 a2)
    (cond
      [(and (eq? a1 'null) (eq? a2 'null)) 'null]
      [(eq? a1 'null) a2]
      [else a1])))

;LookupValue -> takes in env, variable name, returns the value associated with that variable
;returns 'null if there is no value associated with that variable
(define LookupValue
  (lambda (env varName)
    (cond
      [(null? env) 'null]
      [(IsNewLayer? env) (ResolveMultiLayerSearch (LookupValue (GetFirstLayer env) varName) (LookupValue (RemoveLayer env) varName))]
      [(eq? (GetFirstBindingName env) varName) (GetFirstBindingValue env)]
      [else (LookupValue (cdr env) varName)])))

(define AddFunctionDecToEnv
  (lambda(env funcname funcparams funcbody)
    (AddNewBinding env funcname (list funcparams funcbody))))

;***********************************M-Value Helper Functions*****************************************

;there are three types of expressions, compare expressions, boolean expressions, and integer expressions

;MV_ListOfVars -> takes fexpression: a flattened integer expression that may have variables in it, returns a flattened list of all the variables within that expression
(define MV_ListOfVars
  (lambda (fexpression)
    (cond
      [(null? fexpression) fexpression]
      [(not (var? (car fexpression))) (MV_ListOfVars (cdr fexpression))]
      [else (cons (car fexpression) (MV_ListOfVars (cdr fexpression)))])))

;MV_ConvertVarToVal* -> takes env, expression: an integer expression that may have variables in it, and varList: a list of all the variables in the expression
;converts all the variables into values and returns the list
(define MV_ConvertVarToVal*
  (lambda (env expression varList throw)
    (cond
      [(null? varList) expression]
      [(and (not (eq? (car varList) 'funcall)) (IsVarUndeclared? env (car varList))) (error " variable not defined in an expression variable name: " (car varList))]
      ;[else (MV_ConvertVarToVal* env (replaceall* (car varList) (LookupValue env (car varList)) expression) (cdr varList))])))
      [else (MV_ConvertVarToVal* env (replaceall* (car varList) (M-Value env (car varList) throw) expression) (cdr varList) throw)])))
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

(define M-Function
 (lambda(env func throw)
   (M-FormatFunction (HandleFuncall env func 'M-F STD_BREAK STD_CONT throw))))

(define M-FormatFunction
  (lambda(input)
  (if (or (list? input) (eq? input 'null))
      (error "function did not resolve to a value" input)
      (input))))

;M-Value -> takes in env and a partial statement, ultimately resolves the partial statement down to a value and returns that value could be true, false, or a number
(define M-Value
  (lambda (env val throw)
    (cond
      [(MV_NoProcessingNeeded val) val]
      [(and (list? val) (eq? (car val) 'funcall)) (M-Function env val throw)]
      [(list? val) (M-Expression (MV_ConvertVarToVal* env val (MV_ListOfVars (flatten val)) throw))] ;Evaluate the expression
      [(custom-bool-literal? val) (ConvertToSchemeBool val)]
      [(IsVarUndeclared? env val) (error "Undeclared variable!" val)] ;undeclared variable
      [else (LookupValue env val)]))) ;declared variable that needs to be resolved to a value

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



(interpret "t.txt")
;Test Cases:
;
;(list '1 (eq? (interpret "t1.txt") 150))
;(list '2 (eq? (interpret "t2.txt") -4))
;(list '3 (eq? (interpret "t3.txt") 10))
;(list '4 (eq? (interpret "t4.txt") 16))
;(list '5 (eq? (interpret "t5.txt") 220))
;(list '6 (eq? (interpret "t6.txt") 5))
;(list '7 (eq? (interpret "t7.txt") 6))
;(list '8 (eq? (interpret "t8.txt") 10))
;(list '9 (eq? (interpret "t9.txt") 5))
;(list '10 (eq? (interpret "t10.txt") -39))
;(eq? (interpret "t12.txt") ); should give error
;(eq? (interpret "t13.txt") ) ;should give error
;(eq? (interpret "t14.txt") ) ;should give error
;(eq? (interpret "t15.txt") ) ;should give error
;(list '12 (eq? (interpret "t16.txt") 100))
;(list '13 (eq? (interpret "t17.txt") 'false))
;(list '14 (eq? (interpret "t18.txt") 'true))
;(list '15 (eq? (interpret "t19.txt") 128))
;(list '16 (eq? (interpret "t20.txt") 12))
;(list '17 (eq? (interpret "t21.txt") 20))
;(list '18 (eq? (interpret "t22.txt") 164))
;(list '19 (eq? (interpret "t23.txt") 32))
;(list '20 (eq? (interpret "t24.txt") 2))
;(eq? (interpret "t25.txt") ) ;should give error
;(list '21 (eq? (interpret "t26.txt") 25))
;(list '22 (eq? (interpret "t27.txt") 21))
;(list '23 (eq? (interpret "t28.txt") 6))
;(list '24 (eq? (interpret "t29.txt") -1))
;(list '25 (eq? (interpret "t30.txt") 789))
;(eq? (interpret "t31.txt") ) ; should return error
;(eq? (interpret "t32.txt") ) ; should return error
;(eq? (interpret "t33.txt") ) ; should return error
;(list '26 (eq? (interpret "t34.txt") 12 )) ;
;(list '27 (eq? (interpret "t35.txt") 125))
;(list '28 (eq? (interpret "t36.txt") 110))
;(list '29 (eq? (interpret "t37.txt") 2000400)) ; not working
;(list '30 (eq? (interpret "t38.txt") 101))
;(eq? (interpret "t39.txt")) ; should return error
;(list '31 (eq? (interpret "t40.txt") 9)) ; 
;(list '32 (eq? (interpret "t41.txt") 5)) ;


;Part 2 General Idea/ List of Features
;2. break, continue, throw
;3. try catch finally 
;
;For scoping, env will now be a more nested list, '(((f 5) (h 6)) (d 4) (y 2) (x 1))
;when a code block begins, start a new layer in env by consing an empty list onto env, '(() (x 3) (r 4) (e 5))
;when a code block ends, pop off layer that corresponds to that code block by removing the car of Mstate
;this means that the order of env will matter
;task 1 is to modify env functions to account for these changes.
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

;There is an env list that is passed nearly everywhere, it will have all variable bindings so '((x 1) (y 3) ...)

;return statement indicates end of the program, env gets changed to (return value) and the program returns the value





