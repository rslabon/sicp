(define (make-table)
  (list `*table*))

(define (get-record key records)
  (if (null? records)
      #f
      (if (eq? (caar records) key)
          (car records)
          (get-record key (cdr records)))))

(define (get-value key table)
  (let ((record (get-record key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (insert-value! key value table)
  (let ((record (get-record key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table))))))

;ex 3.24
(define (n-make-table equal-key?)
  (let ((local-table (list `*table*)))
    (define (get-record key records)
      (if (null? records)
          #f
          (if (equal-key? (caar records) key)
              (car records)
              (get-record key (cdr records)))))
    (define (get-value key)
      (let ((record (get-record key (cdr local-table))))
        (if record
            (cdr record)
            #f)))
    (define (insert-value! key value)
      (let ((record (get-record key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table (cons (cons key value) (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'get-value) get-value)
            ((eq? m 'insert-value!) insert-value!)
            (else (display 'error))))
    dispatch))
           
;(define t (n-make-table eq?))    
;((t 'get-value) 'a)
;((t 'insert-value!) 'a 666)
;((t 'get-value) 'a)
           
;ex 3.25
(define (are-list-equals? list1 list2)
  (cond ((and (null? list1) (null? list2)) #t)
        ((or (null? list1) (null? list2)) #f)
        ((not (eq? (car list1) (car list2))) #f)
        (else (are-list-equals? (cdr list1) (cdr list2)))))

;(define t (n-make-table are-list-equals?))    
;((t 'get-value) (list 'a 'b))
;((t 'insert-value!) (list 'a 'b) 666)
;((t 'get-value) (list 'a 'b))

;ex 3.27

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((results (make-table)))
    (lambda (x)
      (let ((prev-result (get-value x results)))
        (or prev-result (let ((new-result (f x))) (insert-value! x new-result results) new-result))))))

(define mfib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (mfib (- n 1))
                            (mfib (- n 2))))))))

(mfib 120)

                          
