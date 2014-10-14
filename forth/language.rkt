#lang racket
(require "semantics.rkt")
 
(provide dump
         num
         plus
         min
         mul
         div
         dup
         swap
         drop
         pp
         =
         word
         ;call
         (rename-out [my-module-begin #%module-begin]
                     [my-top-interaction #%top-interaction]
                     [my-datum #%datum]))

; Define an anchor within this namespace so that eval can use to
; perform within the right context (aka one that understands (num 1) and so on)
; DONT EDIT
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

; The current-state is a parameter used by the
; rest of this language.
; DONT EDIT
(define current-state (make-parameter (new-state)))

; Just to make sure that numbers are read in as numbers.
; DONT EDIT
(define-syntax-rule (my-datum . v) (#%datum . v))

; This allows users to use the REPL with s-exp syntax.
; DONT EDIT
(define-syntax-rule (my-top-interaction . v)
  (#%top-interaction . v))

; Every module in this language will make sure that it
; uses a fresh state.
; DONT EDIT
(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
    (parameterize ([current-state (new-state)])
       body ...)))
 
; Implemenation of our actual language

(define-syntax-rule (dump)
  (dump-stack (current-state)))

(define-syntax-rule (num v)
  (push-stack v (current-state)))

(define-syntax-rule (word name body ...)
  (set-word name '(body ...) (current-state)))

(define-syntax-rule (plus)
  (push-stack (+ (pop-stack (current-state))
                 (pop-stack (current-state)))
              (current-state)))

(define-syntax-rule (min)
  (push-stack (- (pop-stack (current-state))
                 (pop-stack (current-state)))
              (current-state)))

(define-syntax-rule (mul)
  (push-stack (* (pop-stack (current-state))
                 (pop-stack (current-state)))
              (current-state)))

(define-syntax-rule (div)
  (push-stack (/ (pop-stack (current-state))
                 (pop-stack (current-state)))
              (current-state)))

(define-syntax-rule (dup)
  (let ((top (pop-stack (current-state))))
    (push-stack top (current-state))
    (push-stack top (current-state))))

(define-syntax-rule (swap)
  (let ((top (pop-stack (current-state)))
        (bottom (pop-stack (current-state))))
    (push-stack top (current-state))
    (push-stack bottom (current-state))))

(define-syntax-rule (drop)
  (pop-stack (current-state)))

(define-syntax-rule (pp)
  (write (pop-stack (current-state))))

(define-syntax-rule (=)
  (let ((lhs (pop-stack (current-state)))
        (rhs (pop-stack (current-state))))
    (if (eq? lhs rhs)
        (num 1)
        (num 0))))

