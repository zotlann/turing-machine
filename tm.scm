;symbol which represents blank symbols on the tape
(define *blank-symbol* #\#)

;if #t, remove all blank symbols from the string before returning
;else keep them
(define *remove-blank-symbol?* #t)

;returns a pair with the car being the list with the last 
;element removed and the cdr being the removed element
(define remove-last
  (lambda (lst)
    (define helper
      (lambda (lst so-far)
        (cond
         [(null? (cdr lst))(cons so-far (car lst))]
         (else (helper (cdr lst) (append so-far (list (car lst))))))))
    (helper lst '())))

;constructs a tape with #str# on it and the read-write head
;on the first character in str.
;returns a procedure that takes 1 to 2 arguments
;the first argument is the operation to do on the tape,
;'get-tape: returns a list with the car a list of symbols to the
;left of the read-write head, the cadar being the symbol on the 
;read/write head and the caddr being a list of symbols to the 
;right of the read/write head
;'read returns the symbol on the read-write head
;'write replaces the character on the read-write head with the symbol passed
;'move-right moves the read-write head one symbol to the right
;'move-left moves the read-write head one symbol to the left
;'get-string returns a string of all of the characters on the tape
;if *remove-blank-symbol?* is #t, it removes all *blank-symbol* from the string
;else it keeps them in place
(define make-tape
  (lambda (str)
    (define left (list *blank-symbol*))
    (define head (car (string->list str)))
    (define right (append (cdr (string->list str)) (list *blank-symbol*)))
    (lambda (method . args)
      (cond
       [(eq? method 'get-tape) (list left head right)]
       [(eq? method 'read) head]
       [(eq? method 'write) (set! head (car args))]
       [(eq? method 'move-right) (begin (set! left (append left (list head)))
                                        (set! head (car right))
                                        (if (null? (cdr right))
                                            (set! right (list *blank-symbol*))
                                            (set! right (cdr right))))]
       [(eq? method 'move-left) (let* ((x (remove-last left))
                                       (new-left (car x))
                                       (new-head (cdr x))
                                       (new-right (append (list head) right)))
                                  (set! head new-head)
                                  (set! right new-right)
                                  (if (null? new-left)
                                      (set! left (list *blank-symbol*))
                                      (set! left new-left)))]
       [(eq? method 'get-string)
	(if *remove-blank-symbol?*
	    (string-append (list->string (remove *blank-symbol* left))
			   (list->string (list head)) 
			   (list->string (remove *blank-symbol* right)))
	    (string-append (list->string left) 
			   (list->string (list head)) 
			   (list->string right)))]
       (else (begin
              (display "INVALID METHOD ON TAPE: ")
	      (display method)(newline)))))))

;constructs a tape with initial state q0, final-state qf
;and with a list of transitions tr
;q0 and qf should be quoted symbols that match the initial and final states
;respectively
;tr is an assoc list in the form:
;(current-state read-character (write-character direction resulting-state)
;eg: (q0 . #\A (#\B R q1))
;for a transition function that while in state q0 replaces #\A with #\B
;and moves the read-write head write, and results in state q1
;you can also use the procedure format-transition on a flat list in the same 
;form to result in an appropriate format.
;eg: (format-transition '(q0 #\A #\B R q1)) for the above transition
;returns a procedure that takes 1 argument
;'get-initial-state returns initial-state
;'get-final-state returns final-state
;'get-transitions returns transitions
;'display-transitions prints the transition functions
(define make-tm
  (lambda (q0 qf tr)
    (define initial-state q0)
    (define final-state qf)
    (define transitions tr)
    (lambda (method)
      (cond
       [(eq? method 'get-initial-state) initial-state]
       [(eq? method 'get-final-state) final-state]
       [(eq? method 'get-transitions) transitions]
       [(eq? method 'display-transitions) 
	(display-transitions (map flatten transitions))]
       (else (begin
              (display "INVALID METHOD ON TM: ")
	      (display method)(newline)))))))

;Constructs and returns a function that takes a tape, performs the
;appropriate write and movement operations on that tape and returns
;the state resulting from those operations
;If there is no transition function for the current-state on the 
;symbol at the read-write head of the tape, this returns #f
(define get-transition
  (lambda (machine tape current-state)
    (let* ((read-symbol (tape 'read))
           (write-dir-resulting (assoc (cons current-state read-symbol)
				       (machine 'get-transitions))))
      (if write-dir-resulting
          (set! write-dir-resulting (cdr write-dir-resulting)))
      (if write-dir-resulting
          (lambda (tape)
            (tape 'write (car write-dir-resulting))
            (if (eq? 'L (cadr write-dir-resulting))
                (tape 'move-left)
                (tape 'move-right))
            (caddr write-dir-resulting))
          #f))))

;applies the constructed turing machine tm to a given input string
;returns the string after the turing machine halts if it halts in 
;a final state
;if the turing machine does not halt in a final state, it returns #f
(define apply-tm
  (lambda (tm str)
    (define tape (make-tape str))
    (define helper
      (lambda (tm tape current-state)
        (cond
         [(eq? (tm 'get-final-state) current-state) (tape 'get-string)]
         (else (let ((transition (get-transition tm tape current-state)))
                 (if transition
                     (helper tm tape (transition tape))
                     #f))))))
    (helper tm tape (tm 'get-initial-state))))

;flattens a list, used when printing transitions for a tm
(define flatten
  (lambda (lst)
    (cond 
      [(null? lst) '()]
      [(pair? lst) (append (flatten (car lst)) (flatten (cdr lst)))]
      (else (list lst)))))

;takes a transition function in the form:
;'(current state read-symbol write-symbol direciton resulting-state)
;and returns a transition function in the form:
;'(current-state . read-symbol (write-symbol direction resulting-state))
;used to create correctly formated transition functions to pass into make-tm
(define format-transition
  (lambda (lst)
    (cons (cons (car lst) (cadr lst)) 
	  (list (car (cddr lst))
		(car (cdddr lst))
		(car (cddddr lst))))))

;display the transition functions of a turing machine in the format
;(current-state read-character write-character direction resulting-state)
;.
;.
;.
(define display-transitions
  (lambda (transitions)
    (if (not (null? transitions))
	(begin (display (car transitions))
	       (newline)
	       (display-transitions (cdr transitions))))))
