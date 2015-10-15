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

;ex 3.23

(define (make-deque) (cons `() `()))
(define (empty-deque? deque) (and (null? (car deque)) (null? (cdr deque))))
(define (front-deque deque) (car deque))
(define (rear-deque deque) (cdr deque))
(define (set-front-deque! deque item) (set-car! deque item))
(define (set-rear-deque! deque item) (set-cdr! deque item))

(define (make-node value prev-node next-node) (list value prev-node next-node))
(define (node-value node) (car node))
(define (node-prev node) (cadr node))
(define (node-next node) (caddr node))
(define (set-node-prev! node new-prev-node) (set-car! (cdr node) new-prev-node))
(define (set-node-next! node new-next-node) (set-car! (cdr (cdr node)) new-next-node))

(define (front-insert-deque! deque item)
    (cond ((empty-deque? deque)
           (let ((first-node (make-node item `() `())))
             (set-front-deque! deque first-node)
             (set-rear-deque! deque first-node)))
	  (else
           (let ((new-front-node (make-node item `() (front-deque deque))))
             (set-node-prev! (front-deque deque) new-front-node)
             (set-front-deque! deque new-front-node))))
  deque)

(define (rear-insert-deque! deque item)
    (cond ((empty-deque? deque)
           (let ((first-node (make-node item `() `())))
             (set-front-deque! deque first-node)
             (set-rear-deque! deque first-node)))
	  (else
	   (let ((new-last-node (make-node item (rear-deque deque) `())))
	     (set-node-next! (rear-deque deque) new-last-node)
	     (set-rear-deque! deque new-last-node))))
  deque)

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (display "empty deque!"))
        (else
           (set-front-deque! deque (node-next (front-deque deque)))
           (if (not (null? (front-deque deque)))
               (set-node-prev! (front-deque deque) `())
               (set-rear-deque! deque `()))))
  deque)

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (display "empty deque!"))
        (else
           (set-rear-deque! deque (node-prev (rear-deque deque)))
           (if (not (null? (rear-deque deque)))
               (set-node-next! (rear-deque deque) `())
               (set-front-deque! deque `()))))
  deque)
            
(define q1 (make-deque))
;(empty-deque? q1)
(front-insert-deque! q1 'a)
(front-insert-deque! q1 'b)
(rear-insert-deque! q1 'c)
(rear-insert-deque! q1 'd)
;(front-delete-deque! q1)
;(front-delete-deque! q1)
(rear-delete-deque! q1)
(rear-delete-deque! q1)
(rear-delete-deque! q1)
(rear-delete-deque! q1)


