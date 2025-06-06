#lang racket
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "cpp-lexer.rkt")

(provide parse-cpp)

;; Parser según el estilo de la documentación
(define cpp-parser
  (parser
   (start program)
   (end EOF)
   (error (lambda (tok-ok? tok-name tok-value start end)
            (error (format "Error de sintaxis en línea ~a, columna ~a cerca de '~a'"
                          (position-line start)
                          (position-col start)
                          (if tok-value tok-value tok-name)))))
   (tokens value-tokens empty-tokens)
   (grammar
    (program [(external-declaration) (list $1)]
             [(program external-declaration) (append $1 (list $2))])
    
    (external-declaration
     [(function-definition) $1]
     [(declaration) $1])
    
    (function-definition
     [(type-specifier declarator compound-statement)
      `(function ,$1 ,$2 ,$3)])
    
    (declaration
     [(type-specifier init-declarator-list SEMI)
      `(declare ,$1 ,$2)])
    
    (type-specifier
     [(INT) 'int]
     [(FLOAT) 'float]
     [(VOID) 'void])
    
    (init-declarator-list
     [(init-declarator) (list $1)]
     [(init-declarator-list COMMA init-declarator) (append $1 (list $3))])
    
    (init-declarator
     [(declarator) $1]
     [(declarator ASSIGN initializer) `(init ,$1 ,$3)])
    
    (initializer
     [(assignment-expression) $1])
    
    (declarator
     [(ID) $1]
     [(declarator LBRACK NUMBER RBRACK) `(array ,$1 ,$3)]
     [(declarator LPAREN parameter-list RPAREN) `(func ,$1 ,$3)])
    
    (parameter-list
     [() '()]
     [(parameter-declaration) (list $1)]
     [(parameter-list COMMA parameter-declaration) (append $1 (list $3))])
    
    (parameter-declaration
     [(type-specifier declarator) `(param ,$1 ,$2)])
    
    (compound-statement
     [(LBRACE RBRACE) '()]
     [(LBRACE statement-list RBRACE) $2])
    
    (statement-list
     [() '()]
     [(statement-list statement) (append $1 (list $2))])
    
    (statement
     [(expression SEMI) $1]
     [(compound-statement) $1]
     [(RETURN expression SEMI) `(return ,$2)]
     [(IF LPAREN expression RPAREN statement) `(if ,$3 ,$5)]
     [(IF LPAREN expression RPAREN statement ELSE statement) `(if ,$3 ,$5 ,$7)]
     [(WHILE LPAREN expression RPAREN statement) `(while ,$3 ,$5)]
     [(FOR LPAREN expression SEMI expression SEMI expression RPAREN statement)
      `(for ,$3 ,$5 ,$7 ,$9)])
    
    (expression
     [(assignment-expression) $1])
    
    (assignment-expression
     [(logical-OR-expression) $1]
     [(unary-expression ASSIGN assignment-expression) `(assign ,$1 ,$3)])
    
    (logical-OR-expression
     [(logical-AND-expression) $1]
     [(logical-OR-expression OR logical-AND-expression) `(or ,$1 ,$3)])
    
    (logical-AND-expression
     [(equality-expression) $1]
     [(logical-AND-expression AND equality-expression) `(and ,$1 ,$3)])
    
    (equality-expression
     [(relational-expression) $1]
     [(equality-expression EQ relational-expression) `(eq ,$1 ,$3)]
     [(equality-expression NEQ relational-expression) `(neq ,$1 ,$3)])
    
    (relational-expression
     [(additive-expression) $1]
     [(relational-expression LT additive-expression) `(lt ,$1 ,$3)]
     [(relational-expression GT additive-expression) `(gt ,$1 ,$3)]
     [(relational-expression LTE additive-expression) `(lte ,$1 ,$3)]
     [(relational-expression GTE additive-expression) `(gte ,$1 ,$3)])
    
    (additive-expression
     [(multiplicative-expression) $1]
     [(additive-expression PLUS multiplicative-expression) `(add ,$1 ,$3)]
     [(additive-expression MINUS multiplicative-expression) `(sub ,$1 ,$3)])
    
    (multiplicative-expression
     [(unary-expression) $1]
     [(multiplicative-expression TIMES unary-expression) `(mul ,$1 ,$3)]
     [(multiplicative-expression DIV unary-expression) `(div ,$1 ,$3)]
     [(multiplicative-expression MOD unary-expression) `(mod ,$1 ,$3)])
    
    (unary-expression
     [(postfix-expression) $1]
     [(NOT unary-expression) `(not ,$2)]
     [(MINUS unary-expression) `(neg ,$2)])
    
    (postfix-expression
     [(primary-expression) $1]
     [(postfix-expression LPAREN argument-expression-list RPAREN) `(call ,$1 ,$3)]
     [(postfix-expression LBRACK expression RBRACK) `(array-ref ,$1 ,$3)])
    
    (argument-expression-list
     [() '()]
     [(assignment-expression) (list $1)]
     [(argument-expression-list COMMA assignment-expression) (append $1 (list $3))])
    
    (primary-expression
     [(ID) $1]
     [(NUMBER) $1]
     [(STRING) $1]
     [(LPAREN expression RPAREN) $2]))))

(define (parse-cpp in)
  (port-count-lines! in)
  (cpp-parser (lambda () (cpp-lexer in))))
