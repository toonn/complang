#lang racket
 
(provide (all-defined-out))
 
(define-struct state (stack words) #:mutable)
 
(define (new-state)
  (make-state '() (make-hash)))

; your implemenation of the semantics

(define push-stack
  (lambda (val a-state)
    (set-state-stack! a-state (cons val (state-stack a-state)))))

(define pop-stack
  (lambda (a-state)
    (let ((stack (state-stack a-state)))
      (set-state-stack! a-state (cdr stack))
      (car stack))))

(define dump-stack
  (lambda (a-state)
    (write (state-stack a-state))))

(define get-word
  (lambda (name a-state)
    (hash-ref (state-words a-state) name)))

(define set-word
  (lambda (name body a-state)
    (hash-set! (state-words a-state) name body)))