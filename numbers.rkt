#lang racket

;; Utility functions

(define (square x) (* x x))


;;; Generic utility functions

(define *ops-table* (make-hash))

(define (put oper types procedure)
  (hash-set! *ops-table* (list oper types) procedure))

(define (get oper types)
  (hash-ref *ops-table* (list oper types) #f))

(define *coercion-table* (make-hash))

(define (put-coercion type1 type2 procedure)
  (hash-set! *coercion-table* (list type1 type2) procedure))

(define (get-coercion type1 type2)
  (hash-ref *coercion-table* (list type1 type2) #f))


(define (attach-tag tag datum)
  (cons tag datum))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      #f))

(define (contents datum)
  (cdr datum))

; Simple 
(define (apply-generic-simple op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No such operation" (list op type-tags))))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (if (eq? op 'raise)
              (apply proc (map contents args))
              (drop (apply proc (map contents args))))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (arg1 (car args))
                    (arg2 (cadr args)))
                (let ((type1->type2 (get-coercion type1 type2))
                      (type2->type1 (get-coercion type2 type1)))
                  (cond (type1->type2 
                         (apply-generic op (type1->type2 arg1) arg2))
                        (type2->type1 
                         (apply-generic (list op arg1 arg2)))
                        (else (error "Couldn't find operation -- apply-generic" (list op args))))))
              (error "No such op -- apply-generic" (list op args)))))))
                                    

;;; Packages

;; Coercions

(put-coercion 'scheme-number 'complex (lambda (x) (make-complex-from-real-imag (contents x) 0)))


;; Scheme Numbers Package

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'raise '(scheme-number) (lambda (x) (make-rational x 1)))
  (put 'equ? '(scheme-number scheme-number) (lambda (x y) (= x y)))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done-scheme-number)

;; Rational Package

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ?)
  (put 'raise '(rational) (lambda (x) (make-complex-from-real-imag (/ (numer x) (denom x)) 0)))
  (put 'project '(rational) (lambda (x) (make-scheme-number (floor (/ (numer x) (denom x))))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done-rational)

;; Rectangular Package

(define (install-rectangular-package)
  ;; internal procedures
  (define (make-from-real-imag real imag)
    (cons real imag))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z) (atan (/ (imag-part z) (real-part z))))
  ;; external interface
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make 'rectangular (lambda (real-part imag-part) 
                            (tag (make-from-real-imag real-part imag-part))))
  (put 'make-mag-ang 'rectangular (lambda (magnitude angle)
                                    (tag (make-from-mag-ang magnitude angle))))
  'done-rectangular)

(define (install-polar-package)
  ;; internal procedures
  (define (make-from-real-imag real imag)
    (cons (sqrt (+ (square real) (square imag)))
          (atan (/ imag real))))
  (define (make-from-mag-ang mag ang)
    (cons mag ang))
  (define (real-part z)
    (* (car z) (cos (cdr z))))
  (define (imag-part z)
    (* (car z) (sin (cdr z))))
  (define (magnitude z)
    (car z))
  (define (angle z)
    (cdr z))
  ;; external interface
  (define (tag x) (attach-tag 'polar x))
  (put 'make 'polar (lambda (mag ang)
                      (tag (make-from-mag-ang mag ang))))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  'polar-done)


(define (install-complex-package)
  ;;internal procedures
  (define (add-complex z1 z2)
    (make-rect-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-rect-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-polar-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-polar-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  ;; external interface
  (define (tag x) (attach-tag 'complex x))
  (put 'add '(complex complex) 
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex) equ?)
  (put 'make-rect 'complex (lambda (real imag) (tag ((get 'make 'rectangular) real imag))))
  (put 'make-polar 'complex (lambda (mag ang) (tag ((get 'make 'polar) mag ang))))
  (put 'project '(complex) (lambda (x) (make-rational (real-part x) 1)))
  'done-complex)


;; Generic procedures

(define make-rect-real-imag (lambda (x y) ((get 'make 'rectangular) x y)))

(define make-polar-mag-ang (lambda (x y) ((get 'make 'polar) x y)))

(define (make-complex-from-real-imag real-part imag-part)
  ((get 'make-rect 'complex) real-part imag-part))

(define (make-complex-from-mag-ang magnitude angle)
  ((get 'make-polar 'complex) magnitude angle))

(define (real-part x)
  (apply-generic 'real-part x))

(define (imag-part x)
  (apply-generic 'imag-part x))

(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (add x y)
  (apply-generic 'add x y))

(define (equ? x y)
  (apply-generic 'equ? x y))

(define (raise x)
  (apply-generic 'raise x))

(define (project x)
  (let ((proc (get 'project (list (type-tag x)))))
    (if proc
        (apply-generic 'project x)
        #f)))

(define (drop x)
  (let ((project-proc (get 'project (list (type-tag x)))))
    (if project-proc
        (let ((projection (project x)))
          (let ((raised (raise projection)))
            (if (equ? raised x)
                (drop projection)
                x)))
        x)))


(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-scheme-number-package)
(install-rational-package)

(drop (make-rational 6 1))