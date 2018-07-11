#lang racket

;;

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

;; adder
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond [(and (has-value? a1)
                (has-value? a2))
           (set-value! sum
                       (+ (get-value a1)
                          (get-value a2))
                       me)]
          [(and (has-value? sum)
                (has-value? a2))
           (set-value! a1
                       (- (get-value sum)
                          (get-value a2))
                       me)]
          [(and (has-value? sum)
                (has-value? a1))
           (set-value! a2
                       (- (get-value sum)
                          (get-value a1))
                       me)]))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me))
  (define (me request)
    (cond
      [(eq? request 'I-have-a-value) (process-new-value)]
      [(eq? request 'I-lost-my-value) (process-forget-value)]
      [else (error "Unknown request: ADDER" request)]))

  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

;; multiplier


;;
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)
(set-value! C 25 'user)

(set-value! F 212 'user)

(forget-value! C 'user)

(set-value! F 212 'user)
