# Assignment 2 Part 2
* Authors: Adrian Edralin and Darren Pak

# Files Submitted
*

# Errors and Other Notes
*

# Critical Appraisal
* There are a lot of interesting questions. For example, #:# does not parse, but (#):# does. Why? Does that mean that it would make sense to change the grammar?

/# is the end of the list while (#) is an empty list. If we have a list that is two items and the first item is the end of the list, that does not make sense when programming. So in this case #:# is not allowed to parse. With (#):#, since (#) is an empty list it is allowed to be a list item. This makes (#):# an allowed list that contains one item, an empty list.

* Explain what changes where made in LambdaNat4 in order to accommodate multiple test cases (separated by ;;) in the same file.

* Reflect on the differences between LambdaNat5 and the Calculator. In LambdaNat5, why can't we implement arithmetic using the simple
    ```
    evalCBN (EPlus e1 e2) = (evalCBN e1) + (evalCBN e2)
    ```
    similar to what we have done in the calculator? Are `+,-,*` implemented using call by name or call by value? What could be a reason for this choice?

* Reflect on the differences between LambdaNat5 and Haskell. In your experience from this assignment, how does writing code in LambdaNat5 and Haskell compare? How far did we come in implementing a functional programming language? What is missing? What can you say about how one should go about extending LambdaNat5 to a more powerful functional language (such as Haskell)?

* Did you notice that `weave` satisfies the invariant "the output-list is sorted if the input-lists are sorted"? Can you use it to prove the correctness of `sort` (that is, that `sort` actually does sort)?
