#lang racket
(require "simpleParser.rkt")






;Abstractions



;Main Interpreter function

;Accepts a filename ex: (interpret "testProgram.txt")
;Outputs the return statement of the Simple Program

(define interpret
  (lambda (filename)
    (parser filename)))

;Quick Run:
(interpret "testProgram.txt")
