(define get-operator (lambda (op-symbol)
    (cond
        ((eq? op-symbol '+) +)
        ((eq? op-symbol '*) *)
        ((eq? op-symbol '-) -)
        ((eq? op-symbol '/) /)
        (else #f)
    )
  )
)

(define letstar? (lambda (e)
    (if (and (list? e) (= (length e) 3) (eq? (car e) 'let*))  
        #t
        #f
    )
  )
)


(define if? (lambda (e)
    (if(and (list? e) (= (length e) 4) (eq? (car e) 'if))
        #t  
        #f
    )
  )
)



(define correctparam? (lambda (e)
    (if (and (list? e)  (= (length (car e)) 2) (list? (car e)) (symbol? (caar e)))
        (if (null? (cdr e))
            #t
            (correctparam? (cdr e)))
        #f)
    )
)


(define let? (lambda (e)
    (if (and (list? e) (= (length e) 3) (eq? (car e) 'let) (if (eq? '() (cadr e)) #t (correctparam? (cadr e))))
        #t
        #f
    )
  )
)


(define elsefin? (lambda (e)
      (if (equal? (car (car(reverse e))) 'else)
          #t
          #f
      )
   )
)


(define elseone?
  (lambda (e)
    (cond 
        ((and (eq? (caar e) 'else) (null? (cdr e))) #t)
        ((and (eq? (caar e) 'else) (not (null? (cdr e)))) #f)
        (else (elseone? (cdr e)))
    )
  )
)


(define cond? (lambda (e)
    (if (and (> (length e) 2) (eq? (car e) 'cond) (list? (cadr e)) (elsefin? e) (elseone? (cdr e)))
        #t
        #f
    )
  )
)


(define isCondElse? (lambda (e)
    (if (and (eq? (length e) 2) (list? (cadr e))  (eq? (caadr e) 'else))
        #t
        #f
    )
  )
)

(define get-value (lambda (var env)
    (cond 
       ( (null? env) (display "cs305: ERROR\n\n") (repl env))
       ( (eq? var (caar env)) (cdar env))
       ( else (get-value var (cdr env)))
    )
  )
)

(define extend-env (lambda (var val old-env)
        (cons (cons var val) old-env)))

(define define-expr? (lambda (e)
         (and (list? e) (= (length e) 3) (eq? (car e) 'define) (symbol?(cadr e)))))

(define s7 (lambda (e env)
   (cond

    ((number? e) e)

    ((symbol? e) (get-value e env))

     ((not (list? e)) (display "cs305: ERROR\n\n") (repl env))

    ((if? e) 
        (if (not(eq? (s7 (cadr e) env) 0 ))
        (s7 (caddr e) env)
        (s7 (cadddr e) env))
    )

    ((cond? e)
        (if(not(eq? (s7 (caadr e) env) 0))
        (s7 (cadadr e) env)
        (s7 (cons 'cond (cddr e)) env))
    )

    ((isCondElse? e)
        (s7 (cadadr e) env)
    )

    ((let? e)
    (let*
        ((parameter (map s7 (map cadr (cadr e)) 
        (make-list (length (map cadr (cadr e))) env)))
        (newenv (append ( map cons (map car (cadr e)) parameter) env)))
        (s7 (caddr e) newenv))
    )

    ((letstar? e)
        (if (eq? (length (cadr e)) 0)
            (s7 (list 'let '() (caddr e)) env)
            (if (eq? (length (cadr e)) 1)
                  (s7 (list 'let (cadr e) (caddr e)) env)
                    (let*
                        ((parameter (s7 (car (cdaadr e)) env))
                        (newenv (cons (cons (caaadr e) parameter) env)))
                        (s7 (list 'let* (cdadr e) (caddr e)) newenv)
                    )
            )
        )
    )

    ((get-operator(car e))
      (let ((operands (map s7 (cdr e) (make-list (length (cdr e)) env))) 
      (operator (get-operator (car e))))
      (apply operator operands))
    )
      (else 
          (display "cs305: ERROR\n\n") (repl env)
      )
    )          
  )            
)

(define repl (lambda (env)
   (let* (
           (dummy1 (display "cs305> "))
           (expr (read))
           (new-env (if (define-expr? expr) 
                        (extend-env (cadr expr) (s7 (caddr expr) env) env)
                        env
                    ))
           (val (if (define-expr? expr)
                    (cadr expr)
                    (s7 expr env)
                ))
           (dummy2 (display "cs305: "))
           (dummy3 (display val))
           (dummy4 (newline))
           (dummy5 (newline))
          )
          (repl new-env)
     )
    )
)
(define cs305 (lambda () (repl '())))