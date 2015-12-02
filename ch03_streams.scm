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
  (cons-stream 1 (delay (integrate-series sine-series))))
(define sine-series
  (cons-stream 0 (delay (stream-map (lambda(x) (* x -1)) (integrate-series cosine-series)))))


;(n-display-stream sine-series 10)

;ex 3.60

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (delay (add-streams 
                       (stream-scale (stream-cdr s1) (stream-car s2))
                       (mul-series s1 (stream-cdr s2))))))

(define r (add-streams (mul-series sine-series sine-series) (mul-series cosine-series cosine-series)))
(n-display-stream r 10)