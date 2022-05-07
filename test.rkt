#lang racket

(require "Kanren/mk.rkt")
; (require "pmatch.rkt")

(run* (q r s) (== q s) (== r s) (== r 2))
(run* (q) (fresh (r s) (== q s) (== r s) (== r 2)))

(run* (q r) (conde ((== q 5) (== 6 r)) ((== q 5))))

(run* (x y) (== `(,x 5) `(3 ,y)))
(run* (x y) (== `(,x 5) y))
(run* (x y) (== `(λ (,x) ,x) `(λ (,y) ,y)))

; (define caro (λ (l o) ((fresh (a d)
                ; (== (cons a d) l)
                ; (== a o)))))
; 
; (define cdro (λ (l o) ((fresh (a d)
                ; (== (cons a d) l)
                ; (== d o)))))


(define append (λ (α β) (cond ((null? α) β) (else (cons (car α) (append (cdr α) β))))))
(define appendo
    (λ (α β o)
        (conde
            ([== '() α] (== β o))
            ((fresh (a d tmp)
                (== (cons a d) α)
                    (== (cons a tmp) o)
                    (appendo d β tmp))))))

(run* (x) (appendo `(1 2) x `(1 2 3 4)))
(run* (x y) (appendo  x y `(1 2 3 4)))
(run 10 (x y z) (appendo x y z))

(define member (λ (α l) (cond ((null? α) false) ((equal? (car l) α) l) (else (member α (cdr l))))))
(define membero
    (λ (x l o)
        (conde
            ; ((== '() l) (== o false))
            ((fresh (a d)
                (== (cons a d) l)
                (== a x))
                (== l o))
            ((fresh (a d)
                (== (cons a d) l)
                (=/= a x)
                (membero x d o))))))

(run* (q x) (membero  x `(1 ,x 3 4) q))

; -------------------------------------
; call-by-value environment-passing λ calculus interpreter in Racket

(define lookup
    (λ (x env)
        (cond
            ((null? env) (error 'lookup "unbound variable"))
            ((equal? (car (car env)) x) (cdr (car env)))
            (else (lookup x (cdr env))))))

; lexical scope
(define interpret
    (λ (expr env)
        (match expr
            (`,x
                #:when (symbol? x)
                (lookup x env)
            )
            (`(λ (,x) ,body)
                `(closure ,x ,body ,env)
            )
            (`(,a ,d)
                (let ((proc (interpret a env))
                     (arg (interpret d env)))
                     (match proc
                        (`(closure ,x ,body ,env)
                            (interpret body `((,x . ,arg) . ,env))
                        )
                        (else
                            (error 'interpret "not a closure"))))))))

; dynamic scope
(define interpret_dyn
    (λ (expr env)
        (match expr
            (`,x
                #:when (symbol? x)
                (lookup x env)
            )
            (`(λ (,x) ,body)
                `(closure ,x ,body)
            )
            (`(,a ,d)
                (let ((proc (interpret_dyn a env))
                     (arg (interpret_dyn d env)))
                     (match proc
                        (`(closure ,x ,body)
                            (interpret_dyn body `((,x . ,arg) . ,env))
                        )
                        (else
                            (error 'interpret_dyn "not a closure"))))))))


; (define inter
    ; (λ (expr env)
        ; (pmatch expr
            ; [`,x (guard (symbol? x))
            ; 'var
            ; ]
            ; [`(λ (,x) ,body)
            ; 'abst
            ; ]
            ; [`(,a ,d)
            ; 'app
            ; ])))

; -------------------------------------
; call-by-value environment-passing λ calculus interpreter in Kanren

(define lookupo
    (λ (x env o)
        (fresh (a d envo)
            (== `((,a . ,d) . ,envo) env)
            (symbolo x) (symbolo a)
            (conde
                ((== x a) (== d o))
                ((=/= x a) (lookupo x envo o))))))

; (define interpreto
;     (λ (expr env o)
;         (conde
;             ((symbolo expr)
;                 (lookupo expr env o)
;             ) ; var
;             ((fresh (x body) (== `(λ (,x) ,body) expr)
;                 (== `(closure ,x ,body ,env) o))
;             ) ; abstraction
;             ((fresh (a d arg x body envo) (== `(,a ,d) expr)
;                 (interpreto a env `(closure ,x ,body ,envo))
;                 (interpreto d env arg)
;                 (interpreto body `((,x . ,arg) . ,envo) o)))))) 
;               ; application

(define interpreto
    (λ (expr env o)
        (conde
            ((symbolo expr)
                (lookupo expr env o)
            ) ; var
            ((fresh (x body) (== `(λ (,x) ,body) expr)
                (symbolo x)
                (== `(closure ,x ,body ,env) o))
            ) ; abstraction
            ((fresh (a d arg x body envo) (== `(,a ,d) expr)
                (interpreto a env `(closure ,x ,body ,envo))
                (interpreto d env arg)
                (interpreto body `((,x . ,arg) . ,envo) o)))))) 
              ; application

; -------------------------------------
; natural numbers

(define nato
    (λ (n)
        (conde
            ((== n 'z))
            ((fresh (n-1)
                (== n `(,'succ ,n-1))
                (nato n-1))))))

(define pluso
    (λ (n m o)
        (conde
            ((== n 'z) (== m o))
            ((fresh (n-1 o-1)
                (== n `(,'succ ,n-1))
                (== o `(,'succ ,o-1))
                (pluso n-1 m o-1))))))

; (define mulo
;     (λ (n m o)
;         (conde
;             ((== n '0) (== '0 o))
;             ((fresh (n-1 o^)
;                 (== n `(,'s ,n-1))
;                 (== o `(+ ,m ,o^))
;                 (mulo n-1 m o^))))))
