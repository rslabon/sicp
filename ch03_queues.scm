(define (make-queue) (cons `() `()))

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item `())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (else
	   (set-cdr! (rear-ptr queue) new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "DELETE! called with an empty queue" queue))
	(else
	 (set-front-ptr! queue (cdr (front-ptr queue)))
	 queue)))

;ex 3.21
(define (print-queue queue)
  (define (print-iter pair)
    (cond ((null? pair)
	   (display "empty")
	   queue)
	  (else
	   (display (car pair))
	   (cond ((not (null? (cdr pair)))
		  (display " -> ")
		  (print-iter (cdr pair))))
	   queue)))
  (print-iter (front-ptr queue)))

;ex 3.22
(define (make-queue)
  (let ((front-ptr `())
	(rear-ptr `()))
    (define (empty?)
      (null? front-ptr))
    (define (insert! item)
      (cond ((empty?)
	     (set! front-ptr (cons item `()))
	     (set! rear-ptr front-ptr)
	     front-ptr)
	    (else
	     (set-cdr! rear-ptr (cons item `()))
	     (set! rear-ptr (cdr rear-ptr))
	     front-ptr)))
    (define (delete!)
      (cond ((empty?)
	     (error "queue is empty!"))
	    (else
	     (set! front-ptr (cdr front-ptr))
	     front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'empty?) empty?)
	    ((eq? m 'insert!) insert!)
	    ((eq? m 'front-ptr) front-ptr)
	    ((eq? m 'rear-ptr) rear-ptr)
	    ((eq? m 'delete!) delete!)
	    (else
	     (error "Unknown invocation!"))))
    dispatch))

;(define q1 (make-queue))
;((q1 'empty?))
;((q1 'insert!) 'a)
;((q1 'insert!) 'b)
;((q1 'insert!) 'c)
;((q1 'delete!))
;((q1 'delete!))




