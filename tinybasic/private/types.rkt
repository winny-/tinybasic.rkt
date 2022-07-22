#|
Break up racket cycling requires by putting types here.
|#
#lang racket/base

(require racket/list)

(provide (all-defined-out))

(struct statement () #:transparent)
(struct statement:empty statement () #:transparent)
(struct statement:print statement (printlist) #:transparent)
(struct statement:input statement (varlist) #:transparent)
(struct statement:let statement (var expression) #:transparent)
(struct statement:goto statement (expression) #:transparent)
(struct statement:gosub statement (expression) #:transparent)
(struct statement:return statement () #:transparent)
(struct statement:if statement (relation left right body) #:transparent)
(struct statement:rem statement (text) #:transparent)
(struct statement:clear statement () #:transparent)
(struct statement:run statement (exprlist) #:transparent)
(struct statement:list statement (exprlist) #:transparent)
(struct statement:end statement () #:transparent)
(struct statement:bye statement () #:transparent)
(struct statement:load statement (filename) #:transparent)
(struct statement:save statement (filename) #:transparent)

(struct function () #:transparent)
(struct function:rnd function (expr) #:transparent)
(struct expression (sign unsignedexpr) #:transparent)
(struct unsignedexpr () #:transparent)
(struct unsignedexpr:unary unsignedexpr (term) #:transparent)
(struct unsignedexpr:add unsignedexpr (a b) #:transparent)
(struct unsignedexpr:sub unsignedexpr (a b) #:transparent)
(struct term () #:transparent)
(struct term:unary term (factor) #:transparent)
(struct term:mul term (a b) #:transparent)
(struct term:div term (a b) #:transparent)
(struct factor (value) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-vars)
  (for/hash ([v (in-list (range 26))])
    (values (integer->char (+ v (char->integer #\A))) 0)))

(define (get-var st var)
  (hash-ref (state-vars st) var))

(struct state (vars program gosubs lineno inputs) #:transparent)
(define clean-state (state (make-vars) (hash) empty #f empty))
