;constructs a turing machine add-tm that takes a string in the form
;"num1+num2" where num1 and num2 are both binary numbers (strings in {0,1}*
;the result of applying add-tm to such a string is the result of adding
;num1 and num2 as a binary integer.
;The algorithm used for the binary addition is as follows:
;check if num1 is zero, if it is return num2
;if num1 is not zero, subtract 1 from num1 and add 1 to num2
;repeat
;The decrement is done by inverting num1, adding 1, and inverting num1 again
;the increment is done by going to the last bit in the number and then 
;inverting all 1's in the number until reaching the first 0 and then
;changing that 0 to 1.
(load "tm.scm")

(define add-trans
  '(
    #| go to the leftmost symbol (start of first digit) |#
    (q0 #\0 #\0 L q0)
    (q0 #\1 #\1 L q0)
    (q0 #\+ #\+ L q0)
    (q0 #\# #\# R q1)
    #| check if the first number is zero |#
    (q1 #\0 #\0 R q1)
    (q1 #\1 #\1 L q2)
    (q1 #\+ #\+ L q9)
    #| go to the leftmost symbol, knowing the first number is not 0 |#
    (q2 #\0 #\0 L q2)
    (q2 #\1 #\1 L q2)
    (q2 #\# #\# R q3)
    #| take 1's complement of the first number |#
    (q3 #\0 #\1 R q3)
    (q3 #\1 #\0 R q3)
    (q3 #\+ #\+ L q4)
    #| add 1 to the 1's complement of the first number |#
    (q4 #\0 #\1 L q5)
    (q4 #\1 #\0 L q4)
    (q4 #\# #\1 R q5)
    #| go the leftmost digit in the first number |#
    (q5 #\0 #\0 L q5)
    (q5 #\1 #\1 L q5)
    (q5 #\# #\# R q6)
    #| take the 1's complement again |#
    (q6 #\0 #\1 R q6)
    (q6 #\1 #\0 R q6)
    (q6 #\+ #\+ R q7)
    #| move to the leftmost digit in the second number |#
    (q7 #\0 #\0 R q7)
    (q7 #\1 #\1 R q7)
    (q7 #\# #\# L q8)
    #| add 1 to the second number and loop|#
    (q8 #\0 #\1 L q0)
    (q8 #\1 #\0 L q8)
    (q8 #\+ #\1 L q11)
    #| if needed, write the carry digit and shift the first number left |#
    (q11 #\0 #\+ L q12)
    (q11 #\1 #\+ L q13)
    (q12 #\0 #\0 L q12)
    (q12 #\1 #\0 L q13)
    (q12 #\# #\0 R q0)
    (q13 #\0 #\1 L q12)
    (q13 #\1 #\1 L q13)
    (q13 #\# #\1 R q0)
    #| go to the leftmost symbol, knowing the first number is 0 |#
    (q9 #\0 #\0 L q9)
    (q9 #\# #\# R q10)
    #|replace the first digit and the + with blanks, leaving only the result |#
    (q10 #\0 #\# R q10)
    (q10 #\+ #\# R qf)))

(define add-tm (make-tm 'q0 'qf add-trans))
(define add-str "1010011+011011")
(display add-str)(display "->")(display (add-tm add-str))(newline)
