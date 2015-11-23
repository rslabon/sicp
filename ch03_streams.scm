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
(stream-ref fib 5)




