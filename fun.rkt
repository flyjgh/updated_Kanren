#lang racket

(require "Kanren/mk.rkt")
; (require "Kanren/pmatch.rkt")

(define caro (λ (l o) ((fresh (a _)
                (== (cons a _) l)
                (== a o)))))

(define cdro (λ (l o) ((fresh (_ d)
                (== (cons _ d) l)
                (== d o)))))

(define app
    (λ (α β o)
        (conde
            ([== '() α] (== β o))
            ((fresh (a d tmp)
                (== (cons a d) α)
                    (== (cons a tmp) o)
                    (app d β tmp))))))

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

(define ℕ
    (λ (n)
        (conde
            ((== n '0))
            ((fresh (n-1)
                (== n `(,'s ,n-1))
                (ℕ n-1))))))

(define pluso
    (λ (n m o)
        (conde
            ((== n '0) (== m o))
            ((fresh (n-1 o-1)
                (== n `(,'s ,n-1))
                (== o `(,'s ,o-1))
                (+ n-1 m o-1))))))
