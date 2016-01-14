(define the-empty-stream `())
(define (stream-null? stream)
  (eq? stream the-empty-stream))

(define (cons-stream a b)
  (cons a b))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-ref stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))

(define (stream-for-each proc stream)
  (if (stream-null? stream)
      'done
      (begin (proc (stream-car stream))
	     (stream-for-each proc (stream-cdr stream)))))

(define (display-stream stream)
  (stream-for-each display-line stream))

(define (n-display-stream stream n)
  (begin (display-line (stream-car stream))
         (if (not (= n 0)) (n-display-stream (stream-cdr stream) (- n 1)))))
  
      

(define (display-line x)
  (newline)
  (display x))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (delay (stream-filter pred (stream-cdr stream)))))
	(else (stream-filter pred (stream-cdr stream)))))

(define (stream-list items)
  (if (null? items)
      the-empty-stream
      (cons-stream (car items) (delay (stream-list (cdr items))))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
		   (delay (stream-enumerate-interval (+ low 1) high)))))

(define (stream-scale stream factor)
  (cons-stream (* factor (stream-car stream))
               (delay (stream-scale (stream-cdr stream) factor))))

;ex 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (delay (apply stream-map
              (cons proc (map stream-cdr argstreams)))))))

(define s1 (stream-list (list 1 2 3)))
(define s2 (stream-list (list 10 20 30)))
(define s3 (stream-list (list 100 200 300)))

(define s4 (stream-map + s1 s2 s3))
;(display-stream s4)

;ex 3.51

(define (show x)
  (display-line x)
  x)

;(define x (stream-map show (stream-enumerate-interval 1 10)))
;(stream-ref x 5)
;(stream-ref x 7)


;infinite-streams

(define (integers-start-from n)
  (cons-stream n
               (delay (integers-start-from (+ n 1)))))

(define integers (integers-start-from 1))

(define (fibgen a b)
  (cons-stream a
               (delay (fibgen b (+ a b)))))
(define fib (fibgen 0 1))

(define (divisible? x y)
  (= 0 (remainder x y)))

(define (sive stream)
  (cons-stream (stream-car stream)
               (delay (sive (stream-filter (lambda(x) (not (divisible? x (stream-car stream)))) (stream-cdr stream))))))
(define primes (sive (integers-start-from 2)))
;(stream-ref primes 50)

(define ones (cons-stream 1 (delay ones)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (delay (add-streams ones integers))))

(define fib
  (cons-stream 0
               (delay (cons-stream 1
                                   (delay (add-streams (stream-cdr fib)
                                                       fib))))))
;(stream-ref fib 5)

;ex 3.53
(define s (cons-stream 1 (delay (add-streams s s))))
;(stream-ref s 2)

;ex 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorial (cons-stream 1 (delay (mul-streams (integers-start-from 2) factorial))))
;(stream-ref factorial 4) ;120

;ex 3.55

(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (delay (add-streams (stream-cdr stream) (partial-sums stream)))))

;(define p (partial-sums integers))
;(stream-ref p 0)
;(stream-ref p 1)
;(stream-ref p 2)
;(stream-ref p 3)
;(stream-ref p 4)

;ex 3.56

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((s1-car (stream-car s1))
                    (s2-car (stream-car s2)))
                (cond ((= s1-car s2-car) (cons-stream s2-car (delay (merge (stream-cdr s1) (stream-cdr s2)))))
                      ((< s1-car s2-car) (cons-stream s1-car (delay (merge (stream-cdr s1) s2))))
                      (else (cons-stream s2-car (delay (merge s1 (stream-cdr s2))))))))))

;(define S (cons-stream 1 (delay (merge (merge (stream-scale S 2) (stream-scale S 3)) (stream-scale S 5)))))

;(n-display-stream S 20)

;ex 3.58

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (delay (expand (remainder (* num radix) den) den radix))))
                
;(define x (expand 1 7 10));1/7 https://en.wikipedia.org/wiki/142857_(number)
;(n-display-stream x 7)
;(define x (expand 3 8 10));3/8
;(n-display-stream x 7)

;ex 3.59

(define (integrate-series stream)
  (define (iter stream n)
    (cons-stream (* (/ 1 n) (stream-car stream))
                 (delay (iter (stream-cdr stream) (+ n 1)))))
  (iter stream 1))

(define exp-series
  (cons-stream 1 (delay (integrate-series exp-series))))

(define cosine-series
  (cons-stream 1 (delay (integrate-series (stream-scale sine-series -1)))))
(define sine-series
  (cons-stream 0 (delay (integrate-series cosine-series))))


;(n-display-stream cosine-series 10)

;ex 3.60

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (delay (add-streams 
                       (stream-scale (stream-cdr s1) (stream-car s2))
                       (mul-series s1 (stream-cdr s2))))))

(define r (add-streams (mul-series sine-series sine-series) (mul-series cosine-series cosine-series)))
;(n-display-stream r 3)

;ex 3.61

(define (invert-unit-series series)
  (cons-stream 1
               (delay (mul-series (stream-scale (stream-cdr series) -1) (invert-unit-series series)))))

(define invert-exp-series (invert-unit-series exp-series))

(define (acc-stream init proc stream n)
  (define (iter acc i s)
    (if (= n i)
        acc
        (iter (proc acc (stream-car s)) (+ i 1) (stream-cdr s))))
  (iter init 0 stream))

;(exact->inexact (acc-stream 0 + invert-exp-series 10)) ; 1/e ~= 0.36787

;ex 3.62

(define (div-series s1 s2)
   (let ((car-s1 (stream-car s1))
         (car-s2 (stream-car s2)))
     (if (= car-s2 0)
         (display "error - 0 div")
         (cons-stream (/ car-s1 car-s2)
                      (delay (mul-series (stream-cdr s1) (invert-unit-series s2)))))))

;(define tg-series (div-series sine-series cosine-series))
;(n-display-stream tg-series 10)   

;3.5.3  Exploiting the Stream Paradigm

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improv guess x)
  (average guess (/ x guess)))
        
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (delay (stream-map (lambda(guess) (sqrt-improv guess x))
                                    guesses))))
  guesses)

(define (square a)
  (* a a))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (delay (euler-transform (stream-cdr s))))))

(define (make-tableau transform s)
  (cons-stream s
               (delay (make-tableau transform
                             (transform s)))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

;(n-display-stream (accelerated-sequence euler-transform (sqrt-stream 2)) 5)

;ex 3.64

(define (stream-limit stream tolerance)
  (let ((s0 (stream-ref stream 0))
        (s1 (stream-ref stream 1)))
    (if (< (abs (- s1 s0)) tolerance)
        s1
        (stream-limit (stream-cdr stream) tolerance))))

(define (my-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;(my-sqrt 2 0.001)

;ex 3.65

(define (ln2-summands n)
    (cons-stream (/ 1.0 n)
                 (delay (stream-map - (ln2-summands (+ n 1))))))

(define ln2 (partial-sums (ln2-summands 1)))

; ln2 ~= 0.693147180559945309417232121458

;(n-display-stream ln2 600); 0.6939784351809084 - still not yet

;(n-display-stream (accelerated-sequence euler-transform ln2) 10)

; INFINITE STREAMS OF PAIRS

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (delay (interleave s2 (stream-cdr s1))))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (delay (interleave
           (stream-map (lambda (x) (list (stream-car s) x))
                       (stream-cdr t))
           (pairs (stream-cdr s) (stream-cdr t))))))

; ex 3.67  
(define x (interleave (pairs integers integers)
                      (stream-map (lambda (x) 
                        (if (not (= (car x) (cadr x)))
                            (list (cadr x) (car x))
                            (list (car x) (cadr x))
                            ))
                        (stream-filter (lambda(x) (not (= (car x) (cadr x)))) (pairs integers integers)))))

;(n-display-stream x 10)

; ex 3.68
;(define (pairs s t)
;  (interleave
;   (stream-map (lambda (x) (list (stream-car s) x)) 
;                t)
;   (pairs (stream-cdr s) (stream-cdr t))));hangs because of no delay in the line

; ex 3.69


            
