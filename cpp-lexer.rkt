#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;; Definición de tokens como en la documentación
(define-tokens value-tokens (NUMBER ID STRING))
(define-empty-tokens empty-tokens
  (EOF
   INT FLOAT VOID     ; tipos
   SEMI COLON COMMA   ; ;
   LBRACE RBRACE      ; { }
   LPAREN RPAREN      ; ( )
   LBRACK RBRACK      ; [ ]
   PLUS MINUS TIMES DIV MOD
   AND OR NOT
   EQ NEQ LT GT LTE GTE
   ASSIGN             ; =
   IF ELSE WHILE FOR RETURN
   ))

(provide value-tokens empty-tokens cpp-lexer)

;; Lexer según convenciones de parser-tools/lex
(define-lex-abbrevs
  [letter (:or (:/ "a" "z") (:/ "A" "Z") "_")]
  [digit (:/ "0" "9")]
  [id (:: letter (:* (:or letter digit)))]
  [integer (:+ digit)]
  [float (:: (:+ digit) "." (:+ digit))]
  [string-lit (:: "\"" (:* (:~ "\"")) "\"")]
  [whitespace (:or #\space #\tab #\newline #\return)])

(define cpp-lexer
  (lexer
   [whitespace (cpp-lexer input-port)]
   [(:: "//" (:* (:~ #\newline))) (cpp-lexer input-port)]
   [(:: "/*" (complement (:: any-string "*/" any-string)) "*/") (cpp-lexer input-port)]
   ["int" (token-INT)]
   ["float" (token-FLOAT)]
   ["void" (token-VOID)]
   ["if" (token-IF)]
   ["else" (token-ELSE)]
   ["while" (token-WHILE)]
   ["for" (token-FOR)]
   ["return" (token-RETURN)]
   [id (token-ID (string->symbol lexeme))]
   [integer (token-NUMBER (string->number lexeme))]
   [float (token-NUMBER (string->number lexeme))]
   [string-lit (token-STRING lexeme)]
   ["==" (token-EQ)]
   ["!=" (token-NEQ)]
   ["<=" (token-LTE)]
   [">=" (token-GTE)]
   ["=" (token-ASSIGN)]
   ["+" (token-PLUS)]
   ["-" (token-MINUS)]
   ["*" (token-TIMES)]
   ["/" (token-DIV)]
   ["%" (token-MOD)]
   ["&&" (token-AND)]
   ["||" (token-OR)]
   ["!" (token-NOT)]
   [";" (token-SEMI)]
   [":" (token-COLON)]
   ["," (token-COMMA)]
   ["{" (token-LBRACE)]
   ["}" (token-RBRACE)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["[" (token-LBRACK)]
   ["]" (token-RBRACK)]
   [(eof) (token-EOF)]))

(define (line-comment lexer port)
  (let ([c (read-char port)])
    (unless (or (eof-object? c) (eqv? c #\newline))
      (line-comment lexer port))
    (lexer port)))

(define (block-comment lexer port depth)
  (let ([c (read-char port)])
    (cond
      [(eof-object? c) (error 'block-comment "Comentario no cerrado")]
      [(and (eqv? c #\*) (eqv? (peek-char port) #\/))
       (read-char port)
       (if (= depth 1)
           (lexer port)
           (block-comment lexer port (sub1 depth)))]
      [(and (eqv? c #\/) (eqv? (peek-char port) #\*))
       (read-char port)
       (block-comment lexer port (add1 depth))]
      [else (block-comment lexer port depth)])))
