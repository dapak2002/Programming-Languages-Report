# Assignment 2 Part 2
* Authors: Adrian Edralin and Darren Pak

# Files Submitted
* LambdaNat5.cf
* Interpreter.hs (for LambdaNat5)
* solutions.hs
* solutions.lc
* test.lc

# Errors and Other Notes
* No known errors.

# Critical Appraisal
* There are a lot of interesting questions. For example, #:# does not parse, but (#):# does. Why? Does that mean that it would make sense to change the grammar?

Using # will denote the end of the list while (#) is an empty list. If we have a list that is two items and the first item is the end of the list, that does not make sense when programming. So in this case #:# is not allowed to parse. With (#):#, since (#) is an empty list it is allowed to be a list item. This makes (#):# an allowed list that contains one item, an empty list.

* Explain what changes where made in LambdaNat4 in order to accommodate multiple test cases (separated by ;;) in the same file.

The only changes that were done was adding line 6 (separator Exp ";;" ;) to LambdaNat5.cf and also adding line 8 and 9 to Interpreter.hs. In the grammar we define what constitutes a separate "Program" and in this case we evaluate two different expressions that can be separated by ";;". In the interpreter we write definitions for programs and also write how the functionality of ";;" is interpreted which in our case is to define two separate "Programs". 

* Reflect on the differences between LambdaNat5 and the Calculator. In LambdaNat5, why can't we implement arithmetic using the simple
    ```
    evalCBN (EPlus e1 e2) = (evalCBN e1) + (evalCBN e2)
    ```
    similar to what we have done in the calculator? Are `+,-,*` implemented using call by name or call by value? What could be a reason for this choice?

We cannot implement arithmetic using the above line of code because our input is not always going to an integer or numeric. We are implementing a call by name because the definition of some variables are not always discrete/numeric. For example, Lambda (\) and its functionality does not always refer to a numeric or integer value, therefore we cannot assume that the inputs for `+,-,*` is always going to be integer values. This is why we call by name, so to maintain the functionality of all functions implemented without having to worry about verifying input too much.

* Reflect on the differences between LambdaNat5 and Haskell. In your experience from this assignment, how does writing code in LambdaNat5 and Haskell compare? How far did we come in implementing a functional programming language? What is missing? What can you say about how one should go about extending LambdaNat5 to a more powerful functional language (such as Haskell)?

* Did you notice that `weave` satisfies the invariant "the output-list is sorted if the input-lists are sorted"? Can you use it to prove the correctness of `sort` (that is, that `sort` actually does sort)?

In our example of `weave`, this would only merge the two list togethers, and the state of whether or not it was sorted is completely reliant on the input lists. For example, if the two lists were sorted from least to greatest, then the outcome of `weave` would also be least to greatest. Another example is if the two lists were not in order, then there is a gurantee that the output WILL NOT come out in order. 

# Interesting Notes
Some interesting problems we ran across when creating the interpreter was that based on how we implemented the code there will always be a space in between values. For example, if we wanted the output `0:1:2:3:4:5:#` instead of `0 : 1 : 2 : 3 : 4 : 5 : #`, this was impossible (to our knowledge) due to the way Haskell is parsed. Perhaps this is due to the way we had implemented ":", however the output is not exactly how we would have liked to present it. Another interesting error that we had resolved was with the parentheses. There were some instances when using hd and tl that the outcome would be a list on its own without appending other elements. To fix this, we added the append function in reverse which resolved this issue, but inherently warranted another functionality that was not intended. This made us think critically of other functions that may have been created accidently or with the intent to fix a different function.