(load "tm.scm")
(define reverse-transitions
  '(
    #| go to leftmost symbol  |#
    (q0 #\0 #\0 L q0)
    (q0 #\1 #\1 L q0)
    (q0 #\# #\# R q1)
    #| 0->X 1->Y |#
    (q1 #\0 #\X R q1)
    (q1 #\1 #\Y R q1)
    (q1 #\# #\# L q2)
    #| go to right-most X/Y replace w/ Z if X append 0, else append 1 |#
    (q2 #\0 #\0 L q2)
    (q2 #\1 #\1 L q2)
    (q2 #\Z #\Z L q2)
    (q2 #\X #\Z R q3)
    (q2 #\Y #\Z R q4)
    (q2 #\# #\# R q5)
    #| append 0 loop to q2 |#
    (q3 #\0 #\0 R q3)
    (q3 #\1 #\1 R q3)
    (q3 #\Z #\Z R q3)
    (q3 #\# #\0 L q2)
    #| append 1 loop to q2 |#
    (q4 #\0 #\0 R q4)
    (q4 #\1 #\1 R q4)
    (q4 #\Z #\Z R q4)
    (q4 #\# #\1 L q2)
    #| replace all Z with # |#
    (q5 #\0 #\0 R qf)
    (q5 #\1 #\1 R qf)
    (q5 #\Z #\# R q5)))

(define reverse-tm (make-tm 'q0 'qf (map format-transition reverse-transitions)))
(display "F(w) = w^R: ")
(reverse-tm 'display-transitions)
(define rev-string "100111010110")
(display rev-string)(display "->")
(display (apply-tm reverse-tm rev-string))(newline)
