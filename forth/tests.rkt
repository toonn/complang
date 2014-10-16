#lang racket
(require rackunit)
(require rackunit/gui)
(require rackunit/text-ui)
(require "./semantics.rkt")
(require "./language.rkt")
;(require "./parser.rkt")

;; (define ts1 (test-suite
;;  "semantics.rkt"

;;  (test-case
;;   "push-stack"
;;   (let ([s (new-state)])
;;     (push-stack 1 s)
;;     (check-equal? (length (state-stack s)) 1)
;;     (push-stack 2 s)
;;     (check-equal? (length (state-stack s)) 2))
;;   )
 
;;  (test-case
;;   "pop-stack"
;;   (let ([s (new-state)])
;;     (push-stack 1 s)
;;     (push-stack 2 s)
;;     (check-equal? (pop-stack s) 2)
;;     (check-equal? (pop-stack s) 1))
;;   )
 
;;  (test-case
;;   "set-word"
;;   (let ([s (new-state)])
;;     (set-word "a" '(1 2) s)
;;     (check-true (dict-has-key? (state-words s) "a"))
;;     (set-word "a" '3 s)
;;     (check-true (dict-has-key? (state-words s) "a")))
;;   )
 
;;  (test-case
;;   "get-word"
;;   (let ([s (new-state)])
;;     (set-word "a" '(1 2) s)
;;     (check-equal? (get-word "a" s) '(1 2))
;;     (set-word "a" '3 s)
;;     (check-equal? (get-word "a" s) '3))
;;   )
 
;;  ))

;; (define ts2 (test-suite
;;  "parser.rkt"

;;  (test-case
;;   "num"
;;   (let ([str (open-input-string "1")])
;;     (let ([pe (parse-expr "test.rkt" str)])
;;       (check-true (syntax? pe) #t)
;;       (check-equal? (syntax-source pe) "test.rkt")
;;       (check-equal? (syntax->datum pe) '(num 1))
;;   )))
 
;;  (test-case
;;   "plus"
;;   (let ([str (open-input-string "+")])
;;     (let ([pe (parse-expr "test.rkt" str)])
;;       (check-true (syntax? pe) #t)
;;       (check-equal? (syntax-source pe) "test.rkt")
;;       (check-equal? (syntax->datum pe) '(plus))
;;   )))
 
;;  (test-case
;;   "="
;;   (let ([str (open-input-string "=")])
;;     (let ([pe (parse-expr "test.rkt" str)])
;;       (check-true (syntax? pe) #t)
;;       (check-equal? (syntax-source pe) "test.rkt")
;;       (check-equal? (syntax->datum pe) '(=))
;;   )))
 
;;  (test-case
;;   "word"
;;   (let ([str (open-input-string ": negate 0 swap - ;")])
;;     (let ([pe (parse-expr "test.rkt" str)])
;;       (check-true (syntax? pe) #t)
;;       (check-equal? (syntax-source pe) "test.rkt")
;;       (check-equal? (syntax->datum pe) '(word "negate" (num 0) (swap) (min)))
;;   )))
 
;;  (test-case
;;   "word"
;;   (let ([str (open-input-string "( wiii comments ) : negate 0 swap - ;")])
;;     (let ([pe (parse-expr "test.rkt" str)])
;;       (check-true (syntax? pe) #t)
;;       (check-equal? (syntax-source pe) "test.rkt")
;;       (check-equal? (syntax->datum pe) '(word "negate" (num 0) (swap) (min)))
;;   )))))

(define ts3 (test-suite
 "language.rkt"
 
 ;; (test-case
 ;;  "num"
 ;;  (let ((s (new-state)))
 ;;    (num 5)
 ;;    (check-equal? (length (state-stack s)) 1)
 ;;    (check-equal? (pop-stack s) 5)))
 
 ;; (test-case
 ;;  "word"
 ;;  (let ((s (new-state)))
 ;;    (word "naam" "body")
 ;;    (check-equal? (get-word "naam" s) "body")))
 
 ;; (test-case
 ;;  "plus"
 ;;  (let ((s (new-state)))
 ;;    (num 574)
 ;;    (num 2)
 ;;    (plus)
 ;;    (check-equal? (pop-stack s) 576)))
 
 ;; (test-case
 ;;  "min"
 ;;  (let ((s (new-state)))
 ;;    (num 574)
 ;;    (num 2)
 ;;    (min)
 ;;    (check-equal? (pop-stack s) 572)))
 
 ;; (test-case
 ;;  "mul"
 ;;  (let ((s (new-state)))
 ;;    (num 574)
 ;;    (num 2)
 ;;    (mul)
 ;;    (check-equal? (pop-stack s) 1148)))
 
 ;; (test-case
 ;;  "div"
 ;;  (let ((s (new-state)))
 ;;    (num 574)
 ;;    (num 2)
 ;;    (div)
 ;;    (check-equal? (pop-stack s) 287)))
 
 ;; (test-case
 ;;  "dup"
 ;;  (let ((s (new-state)))
 ;;    (num 8)
 ;;    (check-equal? (length (state-stack s)) 1)
 ;;    (dup)
 ;;    (check-equal? (length (state-stack s)) 2)
 ;;    (check-equal? (pop-stack s) 8)
 ;;    (check-equal? (pop-stack s) 8)))
 
 ;; (test-case
 ;;  "swap"
 ;;  (let ((s (new-state)))
 ;;    (num 2)
 ;;    (num 4)
 ;;    (check-equal? (length (state-stack s)) 2)
 ;;    (swap)
 ;;    (check-equal? (length (state-stack s)) 2)
 ;;    (check-equal? (pop-stack s) 2)
 ;;    (check-equal? (pop-stack s) 4)))
 
 ;; (test-case
 ;;  "drop"
 ;;  (let ((s (new-state)))
 ;;    (num 0)
 ;;    (check-equal? (length (state-stack s)) 1)
 ;;    (drop)
 ;;    (check-equal? (length (state-stack s)) 0)))
 
 ;; (test-case
 ;;  "pp"
 ;;  (let ((s (new-state)))
 ;;    (num 0)
 ;;    (check-equal? (length (state-stack s)) 1)
 ;;    (pp)
 ;;    (check-equal? (length (state-stack s)) 0)))
 
 (test-case
  "="
  (empty-stack)
  (printf "test/=\n")
  (num 5)
  (num 5)
  (=)
  (dump)
  (printf "\n")
  (num 1)
  (num 2)
  (=)
  (dump)
  (printf "\n")
  )))



 
;(define a  5 negate dump"))
;(parse-expr "test.rkt" a)


;(test/gui ts1 ts2)
(run-tests ts3)
