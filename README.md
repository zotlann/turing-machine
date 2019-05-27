# turing-machine
Universal turing machine implementation in scheme:

Usage:  
Construct a list of transition functions for the turing machine each in the form:  
(initial-state read-char write-char direction resulting-state)  
initial-state nad resulting-state are symbols representing the states  
read-char is the character read on the tape  
write-char is the character written on the tape  
direction is the direction the read write head moves aftewrards (either L for left or R for right  
eg. (q0 #\A #\B R q0) for a transitions function that reads the character A, writes the charater B, moves the right, and stays in q0.  
After constructing the list of transitions, pass it into make-tm along with the symbols for the intital and final states.  
eg. (make-tm 'q0 'qf transitions) for a tm with initial state q0, final state qf, and transitions is bound to a list of transition functions.  
The result of make-tm is a function that takes a string and applies the transitions functions to it.  
By default the character representing blanks on the tape is #, this can be changed by changing the *blank-symbol* variable  
By default, the turing machine will remove all blanks from the output string if there were any on the tape, this can be changed by settings the *remove-blank-symbol?* variable to #f.  
Because the result of make-tm is just a function that takes and returns a string, it is very easy to pipe the output of one turing machine as the input to the next. Or create turing machines that are the composite of any number of turing machines.  
For example, if you have a turing machine that increments a binary number called inc, you can create a turing machine that adds 2 to a binary number by calling composite.  
eg. (composite inc inc) returns a function thta increments a binary number twice.  
