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

(struct statement () #:prefab)
(struct statement:empty statement () #:prefab)
(struct statement:print statement (printlist) #:prefab)
(struct statement:input statement (varlist) #:prefab)
(struct statement:let statement (var expression) #:prefab)
(struct statement:goto statement (expression) #:prefab)
(struct statement:gosub statement (expression) #:prefab)
(struct statement:return statement () #:prefab)
(struct statement:if statement (relation left right body) #:prefab)
(struct statement:rem statement (text) #:prefab)
(struct statement:clear statement () #:prefab)
(struct statement:run statement (exprlist) #:prefab)
(struct statement:list statement (exprlist) #:prefab)
(struct statement:end statement () #:prefab)
(struct statement:bye statement () #:prefab)
(struct statement:load statement (filename) #:prefab)
(struct statement:save statement (filename) #:prefab)
(struct statement:racket statement (code) #:prefab)

(struct function () #:prefab)
(struct function:rnd function (expr) #:prefab)
(struct expression (sign unsignedexpr) #:prefab)
(struct unsignedexpr () #:prefab)
(struct unsignedexpr:unary unsignedexpr (term) #:prefab)
(struct unsignedexpr:add unsignedexpr (a b) #:prefab)
(struct unsignedexpr:sub unsignedexpr (a b) #:prefab)
(struct term () #:prefab)
(struct term:unary term (factor) #:prefab)
(struct term:mul term (a b) #:prefab)
(struct term:div term (a b) #:prefab)
(struct factor (value) #:prefab)

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
  (syntax-rules ()
    [(_ id)
     (struct* expression
                  ([unsignedexpr (struct unsignedexpr:unary
                                   ((struct term:unary
                                      ((struct factor
                                         ((? var? id)))))))]))]
    [(_) (expression>>var _)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct line (number text entire) #:prefab)

(define (tb-var-integer? v)
  (and (exact-integer? v)
       (<= -32768 v +32767)))

(define (make-vars)
  (define-values (START END) (values #\A #\Z))
  (for/hash ([v (in-range (char->integer START) (add1 (char->integer END)))])
    (values (integer->char v)
            0)))

(struct state (vars program gosubs lineno inputs) #:prefab)
(define clean-state (state (make-vars) (hash) empty #f empty))

(define/contract (get-var st var)
  (state? var? . -> . tb-var-integer?)
  (hash-ref (state-vars st) (canonicalize-var var)))

(define/contract (set-var st var int)
  (state? var? tb-var-integer? . -> . state?)
  (struct-copy state st [vars (hash-set (state-vars st)
                                        (canonicalize-var var)
                                        int)]))

