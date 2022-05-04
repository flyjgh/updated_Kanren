#lang racket

(require "Kanren/mk.rkt")
(require "Kanren/pmatch.rkt")

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
