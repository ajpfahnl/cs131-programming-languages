#lang racket
(provide (all-defined-out)) ; post @484

;; Program Description
; write a Scheme procedure (expr-compare x y) that compares two Scheme
; expressions x and y, and produces a difference summary of where the two
; expressions are the same and where they differ

; difference summary is also Scheme expression which, if executed in an
; environment where the Scheme variable % is true, has the same behavior as x,
; and otherwise has the same behavior as y

; the summary should use λ in places where one input expression used a lambda
; expression and the other used a λ expression (however, the summary should
; use lambda in places where both input expressions used lambda)

; summary expression uses the same identifiers as the two input expressions
; where they agree, and if x declares the bound variable X in the same place
; where y declares the bound variable Y, the summary expression should declare
; a bound variable X!Y and use it consistently thereafter wherever the input
; expressions use X and Y respectively. (A bound variable is one that is
; declared in an expression by a binding construct such as lambda.)

; limited to the subset of expressions that consists of literal constants,
; identifiers, function calls, the special form (quote s-exp), the special form
; (lambda formals expr) where formals is a list of identifiers, the equivalent
; special form (λ formals body), and the special-form conditional (if test-expr
; expr expr).

; it should use % to represent a subexpression that is #t in LDA's version and
; #f in SVM's version, and should use (not %) to represent the reverse
; situation

;; My procedures

; zip: zips two lists together (inspired by Python3 zip)
; ref: https://stackoverflow.com/questions/21688944/zip-function-in-racket-scheme
(define (zip l1 l2)
(map list l1 l2))

; lambda?: checks if input is lambda or λ
; (define LAMBDA (string->symbol "\u03BB"))
(define (lambda? x)
    (member x '(lambda λ)))

; bindvar: binds two symbols together with '!' in between
(define (bindvar x y) 
    (string->symbol 
        (string-append (symbol->string x) "!" (symbol->string y))))

; ifexpr?: check if we have an if expression
(define (ifexpr? x)
    (and (equal? (length x) 4) (equal? (car x) 'if)))

; bindvar-map: maps symbols in lambda binding with hash table
(define (bindvar-map x y revbool)
    (define (bm-helper b-map x y)
        (cond
            [(null? x) b-map]
            [(equal? (car x) (car y)) (bm-helper b-map (cdr x) (cdr y))]
            [revbool (let ([val (bindvar (car y) (car x))])
                (bm-helper (hash-set b-map (car x) val) (cdr x) (cdr y)))]
            [#t (let ([val (bindvar (car x) (car y))])
                (bm-helper (hash-set b-map (car x) val) (cdr x) (cdr y)))]
        )
    )
    (begin
    ; (displayln (bm-helper (hash) x y)) ; for debugging
    (bm-helper (hash) x y)
    )
)

; replace: replaces symbols based on lambda binding
(define (replace x y b-map)
    (define (replace-helper x y)
        (cond 
            [(null? x) x]
            [(and (list? (car x)) (symbol? (caar x))) (cons
                (replace-helper (car x) (car y))
                (replace-helper (cdr x) (cdr y))
                )]
            [(list? (car x)) (cons
                (cons (caar x) (replace-helper (cdar x) (cdar y)))
                (replace-helper (cdr x) (cdr y))
                )]
            [#t (cons 
                    (hash-ref b-map (car x) (car x)) 
                    (replace-helper (cdr x) (cdr y)))]
        )
    )
    (cons (replace-helper (car x) (car y)) (replace-helper (cdr x) (cdr y)))
)

; expr-compare-map: calls expr-compare below on each subexpression of x and y
(define (expr-compare-map x y)
    (map 
        (lambda (p) (apply (lambda (a b) (expr-compare a b)) p))
        (zip x y)
    )
)

(define (expr-compare x y)
    (cond
        ; check equal values
        [(equal? x y) x]
        ; check if booleans
        [(and (boolean? x) (boolean? y)) (if x '% '(not %))]
        ; check both are lists (e.g. both are functions)
        [(or (not (list? x)) (not (list? y))) (list 'if '% x y)]
        ; check both lists are the same length (e.g. both have same structure)
        [(not (equal? (length x) (length y))) (list 'if '% x y)]
        ; check quotes: the special form (quote s-exp)
        [(and (equal? (car x) (car y)) (equal? (car x) 'quote)) (list 'if '% x y)]
        ; check ifs: the special form (if test-expr expr expr)
        [(and (or (ifexpr? x) (ifexpr? y)) (not (equal? (car x) (car y))))
            (list 'if '% x y)
        ]
        ; check lambda functions: the special form (lambda formals expr)
        [
            (and
                (equal? (length x) 3)
                (and (lambda? (car x)) (lambda? (car y)))
            )
            (let ([b-map-x (bindvar-map (cadr x) (cadr y) #f)]
                  [b-map-y (bindvar-map (cadr y) (cadr x) #t)])
                (let ([cdr-x (replace (cdr x) (cdr y) b-map-x)]
                      [cdr-y (replace (cdr y) (cdr x) b-map-y)])
                    (if (not (equal? (car x) (car y)))
                        (expr-compare-map (cons 'λ cdr-x) (cons 'λ cdr-y))
                        (expr-compare-map (cons (car x) cdr-x) (cons (car x) cdr-y))
                    )
                )
            )
        ]
        ; recurse -> this is sort of Pythonic thinking. I'm zipping the two lists
        ; (functions) and then unpacking the tuples into a call to expr-compare
        ; to check the subexpressions
        [#t (expr-compare-map x y)]
    )
)

; test-expr-compare: straight from TA hint code here:
; https://github.com/CS131-TA-team/UCLA_CS131_CodeHelp/blob/master/Scheme/starting_hint.ss
(define (test-expr-compare x y) 
    (and (equal? (eval x)
                (eval `(let ((% #t)) ,(expr-compare x y))))
        (equal? (eval y)
                (eval `(let ((% #f)) ,(expr-compare x y))))))

; test-expr-x and test-expr-y:
;   covers: constant literals, variables, procedure calls, quote, lambda, if
;   in racket: (test-expr-compare test-expr-x test-expr-y)
(define test-expr-x
    '(if #t 
        (map 
        (lambda (p) (apply (lambda (a b) (list a b)) p))
        (zip '(x "b" 1) '(a "a" 2)))
    12345)
)

(define test-expr-y 
    '(if #f
        (map 
        (λ (p) (apply (lambda (q z) (list q z)) p))
        (zip '(xa "a" 1) '(aa "b" 1)))
    45678)
)

;; TESTING
; (define (dexpr-compare x y) (displayln (expr-compare x y)))

; (dexpr-compare 12 12)
; (dexpr-compare 12 20)
; (dexpr-compare #t #t)
; (dexpr-compare #f #f)
; (dexpr-compare #t #f)
; (dexpr-compare #f #t)
; (dexpr-compare '(/ 1 0) '(/ 1 0.0))
; (dexpr-compare 'a '(cons a b))
; (dexpr-compare '(cons a b) '(cons a b))
; (dexpr-compare '(cons a lambda) '(cons a λ))
; (dexpr-compare '(cons (cons a b) (cons b c))
;     '(cons (cons a c) (cons a c)))
; (dexpr-compare '(cons a b) '(list a b))
; (dexpr-compare '(list) '(list a))
; (dexpr-compare ''(a b) ''(a c)) ; special
; (dexpr-compare '(quoth (a b)) '(quoth (a c)))
; (dexpr-compare '(if x y z) '(if x z z))
; (dexpr-compare '(if x y z) '(g x y z))
; (dexpr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2))
; (dexpr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2))
; (dexpr-compare '((lambda (a) a) c) '((lambda (b) b) d)) ; special
; (dexpr-compare ''((λ (a) a) c) ''((lambda (b) b) d))
; (dexpr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
;               '(+ #t ((lambda (a c) (f a c)) 1 2))) ; special
; (dexpr-compare '((λ (a b) (f a b)) 1 2)
;               '((λ (a b) (f b a)) 1 2))
; (dexpr-compare '((λ (a b) (f a b)) 1 2)
;               '((λ (a c) (f c a)) 1 2))
; (dexpr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3) '((lambda (if) (+ if if (f λ))) 3))
; (dexpr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
;                                     a (lambda (a) a))))
;                 (lambda (b a) (b a)))
;               '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
;                                 a (λ (b) a))))
;                 (lambda (a b) (a b))))