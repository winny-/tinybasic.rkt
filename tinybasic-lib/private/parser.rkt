#|
Parser/lexer stuff.
Plus read/write a program.
|#
#lang racket/base

(require racket/contract/base
         racket/contract/region
         racket/format
         racket/function
         racket/list
         racket/match
         racket/port
         racket/string
         (for-syntax racket/base)

         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc

         readline/pread

         "types.rkt")

(provide (all-defined-out))

(define-lex-trans :ci
  (λ (stx)
    (syntax-case stx ()
      [(_ s)
       (let ([alternations
              (datum->syntax
               stx
               (for/list ([c (in-string (syntax->datum #'s))])
                 `(:+ ,(char-upcase c) ,(char-downcase c))))])
         #`(:: #,@alternations))])))

(define-lex-abbrevs
  (tstring (:: #\" (:* (:~ #\newline #\return #\")) #\"))
  (tdigit (char-range #\0 #\9))
  (tnumber (:+ numeric))
  (trelop (:or #\< #\> #\= "<=" ">=" "<>" "><"))
  (tvar (:or (char-range #\A #\Z) (char-range #\a #\z)))
  (twhitespace (:+ (:or #\tab #\space)))
  (tcr (:or "\r\n" #\newline)))

(define-tokens complex-tokens
  (NUMBER STRING VAR REM))

(define-empty-tokens simple-tokens
  ( ; This blank keeps racket-mode happy
   PRINT IF GOTO INPUT LET GOSUB RETURN CLEAR LIST RUN END EQ LT GT LTE GTE NE
   ADD SUB DIV MUL COMMA SPACE CLOSE-PAREN OPEN-PAREN RND EOF SEMICOLON COLON
   THEN BYE NEWLINE LOAD SAVE))

(define tb-lexer
  (lexer
   [(eof) (token-EOF)]
   [(:: (:ci "REM") (:* (:~ #\newline #\return)))
    (token-REM lexeme)]
   [tvar (token-VAR (string-upcase lexeme))]
   [tnumber (token-NUMBER (string->number lexeme))]
   [tstring (token-STRING (string-replace lexeme "\"" ""))]
   [(:or (:ci "PR") (:ci "PRINT")) (token-PRINT)]
   [(:ci "IF") (token-IF)]
   [(:: (:ci "GO") (:? twhitespace) (:ci "TO")) (token-GOTO)]
   [(:ci "INPUT") (token-INPUT)]
   [(:ci "LET") (token-LET)]
   [(:: (:ci "GO") (:? twhitespace) (:ci "SUB"))  (token-GOSUB)]
   [(:ci "RETURN") (token-RETURN)]
   [(:ci "CLEAR") (token-CLEAR)]
   [(:ci "LIST") (token-LIST)]
   [(:ci "RUN") (token-RUN)]
   [(:ci "END") (token-END)]
   [(:ci "RND") (token-RND)]
   [(:ci "THEN") (token-THEN)]
   [(:ci "BYE") (token-BYE)]
   [(:ci "LOAD") (token-LOAD)]
   [(:ci "SAVE") (token-SAVE)]
   ["=" (token-EQ)]
   ["<" (token-LT)]
   [">" (token-GT)]
   ["<=" (token-LTE)]
   [">=" (token-GTE)]
   [(:or "<>" "><") (token-NE)]
   ["+" (token-ADD)]
   ["-" (token-SUB)]
   ["/" (token-DIV)]
   ["*" (token-MUL)]
   ["," (token-COMMA)]
   [";" (token-SEMICOLON)]
   [":" (token-COLON)]
   ["(" (token-OPEN-PAREN)]
   [")" (token-CLOSE-PAREN)]
   [twhitespace (tb-lexer input-port)]
   [tcr (token-NEWLINE)]))

(define/contract (string->line s)
  (string? . -> . (or/c #f line?))
  (match s
    [(regexp #px"([0-9 ]*)(.*)" (list entire num stmt))
     (line (string->number (string-replace num " " "")) stmt entire)]
    [_ #f]))

(define statement-parser
  (parser
   (tokens complex-tokens simple-tokens)
   (start statement)
   (end EOF NEWLINE) ; Probably safe to omit NEWLINE as this should never be
                     ; used for an entire program.
   (error (λ (tok-ok? tok-name tok-value)
            (raise-syntax-error 'tb-parser (format "~a ~a ~a" tok-ok? tok-name tok-value))))
   (grammar
    (statement [(PRINT printlist) (statement:print $2)]
               [(INPUT exprlist) (statement:input $2)]
               [(LET var EQ expression) (statement:let $2 $4)]
               [(var EQ expression) (statement:let $1 $3)]
               [(GOTO expression) (statement:goto $2)]
               [(GOSUB expression) (statement:gosub $2)]
               [(RETURN) (statement:return)]
               [(IF expression relop expression THEN statement)
                (statement:if $3 $2 $4 $6)]
               [(IF expression relop expression statement)
                (statement:if $3 $2 $4 $5)]
               [(REM) (statement:rem $1)]
               [(CLEAR) (statement:clear)]
               [(RUN) (statement:run empty)]
               [(RUN COMMA exprlist) (statement:run $3)]
               [(LIST) (statement:list empty)]
               [(LIST expression) (statement:list (list $2))]
               [(LIST expression COMMA expression) (statement:list (list $2 $4))]
               [(END) (statement:end)]
               [(BYE) (statement:bye)]
               [(LOAD STRING) (statement:load $2)]
               [(SAVE STRING) (statement:save $2)]
               [() (statement:empty)])
    (printlist [() empty]
               [(printitem) (list $1)]
               [(printitem COLON) (list $1 'colon)]
               [(printitem separator printlist) (cons $1 (cons $2 $3))])
    (printitem [(expression) $1]
               [(STRING) $1])
    (exprlist [(expression) (list $1)]
              [(expression COMMA exprlist) (cons $1 $3)])
    (expression [(unsignedexpr) (expression #f $1)]
                [(ADD unsignedexpr) (expression '+ $2)]
                [(SUB unsignedexpr) (expression '- $2)])
    (unsignedexpr [(term) (unsignedexpr:unary $1)]
                  [(term ADD unsignedexpr)
                   (unsignedexpr:add $1 $3)]
                  [(term SUB unsignedexpr)
                   (unsignedexpr:sub $1 $3)])
    (term [(factor) (term:unary $1)]
          ;; Note term and factor are reversed.  I'm guessing the original
          ;; publication's grammar was slightly incorrect (as per the English
          ;; description in the same publication).
          [(term DIV factor) (term:div $1 $3)]
          [(term MUL factor) (term:mul $1 $3)])
    (factor [(VAR) (factor $1)]
            [(NUMBER) (factor $1)]
            [(OPEN-PAREN expression CLOSE-PAREN) (factor $2)]
            [(function) (factor $1)])
    (function [(RND OPEN-PAREN expression CLOSE-PAREN) (function:rnd $3)])
    (relop [(LT) '<]
           [(GT) '>]
           [(EQ) '=]
           [(LTE) '<=]
           [(GTE) '>=]
           [(NE) '!=])
    (var [(VAR) $1])
    (separator [(COMMA) 'comma]
               [(SEMICOLON) 'semicolon]))))

(define input-parser
  (parser
   (tokens complex-tokens simple-tokens)
   (start exprs-or-none)
   (end EOF NEWLINE) ; Probably safe to omit NEWLINE as this should never be
                     ; used for an entire program.
   (error (λ (tok-ok? tok-name tok-value)
            (raise-syntax-error 'input-parser (format "~a ~a ~a" tok-ok? tok-name tok-value))))
   (grammar
    (exprs-or-none [(exprlist) $1]
                   [() empty])
    (exprlist [(expression) (list $1)]
              [(expression COMMA exprlist) (cons $1 $3)])
    (expression [(unsignedexpr) (expression #f $1)]
                [(ADD unsignedexpr) (expression '+ $2)]
                [(SUB unsignedexpr) (expression '- $2)])
    (unsignedexpr [(term) (unsignedexpr:unary $1)]
                  [(term ADD unsignedexpr)
                   (unsignedexpr:add $1 $3)]
                  [(term SUB unsignedexpr)
                   (unsignedexpr:sub $1 $3)])
    (term [(factor) (term:unary $1)]
          ;; Note term and factor are reversed.  I'm guessing the original
          ;; publication's grammar was slightly incorrect (as per the English
          ;; description in the same publication).
          [(term DIV factor) (term:div $1 $3)]
          [(term MUL factor) (term:mul $1 $3)])
    (factor [(VAR) (factor $1)]
            [(NUMBER) (factor $1)]
            [(OPEN-PAREN expression CLOSE-PAREN) (factor $2)]
            [(function) (factor $1)])
    (function [(RND OPEN-PAREN expression CLOSE-PAREN) (function:rnd $3)])
    (var [(VAR) $1]))))

(define/contract (tb-load [ip (current-input-port)]
                          #:state [init-state clean-state])
  (() (input-port?
       #:state state?) . ->* . state?)
  (struct-copy state init-state
               [program
                (for/hash ([li (port->list tb-read ip)]
                           #:when (line-number li)) ; Omit direct mode lines here.
                  (values (line-number li) li))]))

(define (port->tinybasic-program ip)
  (port->list tb-read ip))

(define (tb-read [ip (current-input-port)])
  (parameterize ([readline-prompt #": "])
    (match (read-line ip)
      [(? eof-object? e) e]
      [text
       (match (string->line text)
         [(struct* line ([number 0] [entire entire]))
          (raise-syntax-error 'tb-read "Line number cannot be zero in `~a'" entire)]
         [(? line? li) li]
         [#f (raise-syntax-error 'tb-read "Bad line `~a'" text)])])))

(define/contract (tb-read-input [ip (current-input-port)])
  (() (input-port?) . ->* . (or/c eof-object? (listof expression?)))
  (parameterize ([readline-prompt #"? "])
    (match (read-line ip)
      [(? eof-object? e) e]
      [text
       (with-input-from-string text
        (thunk
         (input-parser (thunk (tb-lexer (current-input-port))))))])))

(define/contract (parse-line s)
  (-> (or/c string? line?) statement?)
  (when (line? s) (set! s (line-text s)))
  (if (statement? s)
      s
      (with-input-from-string s
        (thunk
         (statement-parser (thunk (tb-lexer (current-input-port))))))))

(define (tb-write ast [output-port (current-output-port)] #:what [what 'all])
  (parameterize ([current-output-port output-port])
    (define lines (map cdr (sort (hash->list ast) < #:key car)))
    (define width
      (add1 (inexact->exact (round (log (if (empty? lines) 1000 (line-number (last lines))) 10)))))
    (define (pline li)
      (printf "~a ~a\n"
              (~r (line-number li) #:min-width width)
              (line-text li)))
    (match what
      ['all (for-each pline lines)]
      [(? exact-positive-integer? n)
       (match (hash-ref ast n #f)
         [#f (void)]
         [li (pline li)])]
      [(list a b)
       (for-each pline (filter (match-lambda [(struct* line ([number n])) (<= a n b)]) lines))])))
