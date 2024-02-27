(define twoOperatorCalculator 
        (lambda (lst)
		(if (null? (cdr lst))
			(car lst)
			(if (eq? '+ (cadr lst))
				(twoOperatorCalculator (cons (+ (car lst) (caddr lst) ) (cdddr lst) ) )
				(if (eq? '- (cadr lst))
					(twoOperatorCalculator (cons (- (car lst) (caddr lst) ) (cdddr lst) ) )
					
                                )
                        )
                )
        )
)

(define fourOperatorCalculator 
        (lambda (lst)
	        (if (null? (cdr lst))
	        lst
	        	(if (eq? '* (cadr lst))
		        	(fourOperatorCalculator (cons (* (car lst) (caddr lst) ) (cdddr lst) ) )					
			         (if (eq? '/ (cadr lst))
                                        (fourOperatorCalculator (cons (/ (car lst) (caddr lst) ) (cdddr lst) ) )
					(cons (car lst) (fourOperatorCalculator (cdr lst) ) )
                                )            
                        )
                )
        )
)

(define calculatorNested
        (lambda (lst)
                (map helperCalculator lst)
        )
)

(define helperCalculator
        (lambda (lst)
                (if (not (pair? lst)) 
                        lst
                        (twoOperatorCalculator (fourOperatorCalculator (calculatorNested lst)))
                )
        )        
)

(define checkOperators
        (lambda (lst)
                (cond
                        ((null? lst) #f)
                        ((and(symbol? lst) (not(list? lst))) #f)
                        ((and(number? lst) (not(list? lst))) #f)
                        ((list? (car lst))
                        (if (null? (cdr lst))
                                (checkOperators(car lst))
                                (and (checkOperators(cdr lst)) (checkOperators(car lst)))
                        )
                        )
                        ((number? (car lst))
                                (if (not(null? (cdr lst)))
                                        (if (not (number? (cadr lst)))
                                                (checkOperators (cdr lst)) 
                                        #f)
                                #t)
                        )
                        ((or (eq? '* (car lst)) (eq? '/ (car lst)) (eq? '+ (car lst)) (eq? '- (car lst)))
                                (if (not (null? (cdr lst)))
                                        (checkOperators (cdr lst))
                                        #f))
                        ((not (and (not (number? (car lst))) (or (eq? '* (car lst)) (eq? '/ (car lst)) (eq? '+ (car lst)) (eq? '- (car lst))) ) ) #f)
                        (else #t)
                )
        )
)

(define calculator
        (lambda (lst)
                (if (checkOperators lst)
                        (twoOperatorCalculator (fourOperatorCalculator(calculatorNested lst)))
                        #f
                )
        )
)