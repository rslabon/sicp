(define (parallel-execute . procs)
  (map thread-wait
       (map (lambda (proc) (thread proc))
            procs)))

(parallel-execute (lambda () (display "A")) (lambda () (display "B")))