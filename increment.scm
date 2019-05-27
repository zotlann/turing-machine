(load "tm.scm")
(define inc-trans
 '(
   #|go to rightmost|#
   (q0 #\0 #\0 R q0)
   (q0 #\1 #\1 R q0)
   (q0 #\# #\# L q1)
   #|add 1 |#
   (q1 #\0 #\1 L qf)
   (q1 #\1 #\0 L q1)
   (q1 #\# #\1 R qf)))

(define inc-tm (make-tm 'q0 'qf inc-trans))
(define add-two (composite inc-tm inc-tm))

(display "Incerementer: ")(newline)
(display "001->") (display (inc-tm "001")) (newline)
(display "+2 constructed using composite and the incrementer")(newline)
(display "010->")(display (add-two "010"))(newline)
