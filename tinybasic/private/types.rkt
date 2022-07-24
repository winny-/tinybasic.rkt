#|
Break up racket cycling requires by putting types here.
|#
#lang racket/base

(require racket/list
         racket/match
         racket/contract/base
         racket/contract/region
         (for-syntax racket/base))

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

(define (var? v)
  (cond [(char? v)
         (or (char<=? #\A v #\Z)
             (char<=? #\a v #\z))]
        [(string? v)
         (and (= 1 (string-length v))
              (var? (string-ref v 0)))]
        [else #f]))

(define (line-number? v) (or (not v)
                             (exact-positive-integer? v)))

(define/contract (canonicalize-var v)
  (var? . -> . char?)
  (if (char? v)
      (char-upcase v)
      (canonicalize-var (string-ref v 0))))

(define-match-expander expression>>var
  (lambda (stx)
    (syntax-case stx ()
      [(_ id)
       #'(struct* expression
                  ([unsignedexpr (struct unsignedexpr:unary
                                   ((struct term:unary
                                      ((struct factor
                                         ((? var? id)))))))]))]
      [(_) #'(expression>>var _)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tb-var-integer? v)
  (and (exact-integer? v)
       (<= -32768 v +32767)))

(define (make-vars)
  (define-values (START END) (values #\A #\Z))
  (for/hash ([v (in-range (char->integer START) (add1 (char->integer END)))])
    (values (integer->char (+ v (char->integer START)))
            0)))

(struct state (vars program gosubs lineno inputs) #:transparent)
(define clean-state (state (make-vars) (hash) empty #f empty))

(define/contract (get-var st var)
  (state? var? . -> . tb-var-integer?)
  (hash-ref (state-vars st) (canonicalize-var var)))

(define/contract (set-var st var int)
  (state? var? tb-var-integer? . -> . state?)
  (struct-copy state st [vars (hash-set (state-vars st)
                                        (canonicalize-var var)
                                        int)]))

