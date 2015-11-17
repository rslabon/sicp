(define (stream-ref stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream (- n 1)))))

(define (stream-for-each proc stream)
  (if (stream-null? stream)
      'done
      (begin (proc (stream-car stream))
	     (stream-for-each proc (stream-cdr stream)))))

(define (display-stream stream)
  (stream-for-each (lambda(i) (newline)(display i)) stream))

(define (stream-filter pred stream)
  (cond ((stream-null?) the-empty-stream)
	((pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred (stream-cdr stream))))
	(else (stream-filter pred (stream-cdr? stream)))))

(define (stream-list items)
  (if (null? items)
      the-empty-stream
      (cons-stream (car items) (stream-list (cdr items)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
		   (stream-enumerate-interval (+ low 1) high))))

;ex 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define s1 (stream-list (list 1 2 3)))
(define s2 (stream-list (list 10 20 30)))
(define s3 (stream-list (list 100 200 300)))

(define s4 (stream-map + s1 s2 s3))
(display-stream s4)
