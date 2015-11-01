; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;
; Claude Anderson.  Last modified April, 2014

;(load "chez-init.ss")
(load "C:/Users/efronbs/Documents/School/y3q1/plc/chez-init.ss")


(define load-all ; make it easy to reload the files
  (lambda ()
    (load "C:/Users/efronbs/Documents/School/y3q1/plc/Interpreter/datatypes.ss")
    (load "C:/Users/efronbs/Documents/School/y3q1/plc/Interpreter/parse.ss")
    (load "C:/Users/efronbs/Documents/School/y3q1/plc/Interpreter/syntax-expand.ss")
    (load "C:/Users/efronbs/Documents/School/y3q1/plc/Interpreter/env.ss")
    (load "C:/Users/efronbs/Documents/School/y3q1/plc/Interpreter/interpreter.ss")))

 ;	  (load "datatypes.ss")
 ;    (load "parse.ss")
 ;    (load "syntax-expand.ss")
 ;    (load "env.ss")
 ;    (load "interpreter.ss")))

(load-all)

(define l load-all) ; even easier!
