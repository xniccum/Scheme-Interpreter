; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;
; Claude Anderson.  Last modified April, 2014



(define load-all ; make it easy to reload the files
  (lambda ()
    (load "C:/Users/efronbs/Documents/School/y3q1/plc/New/Scheme-Interpreter/chez-init.ss")
    (load "C:/Users/efronbs/Documents/School/y3q1/plc/New/Scheme-Interpreter/datatypes.ss")
    (load "C:/Users/efronbs/Documents/School/y3q1/plc/New/Scheme-Interpreter/parse.ss")
    (load "C:/Users/efronbs/Documents/School/y3q1/plc/New/Scheme-Interpreter/syntax-expand.ss")
    (load "C:/Users/efronbs/Documents/School/y3q1/plc/New/Scheme-Interpreter/env.ss")
    (load "C:/Users/efronbs/Documents/School/y3q1/plc/New/Scheme-Interpreter/interpreter.ss")))
  ;   (load "chez-init.ss")
 	; (load "datatypes.ss")
  ;   (load "parse.ss")
  ;   (load "syntax-expand.ss")
  ;   (load "env.ss")
  ;   (load "interpreter.ss")))

(load-all)

(define l load-all) ; even easier!
