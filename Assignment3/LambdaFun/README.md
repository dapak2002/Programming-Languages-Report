# Assignment 3
* Authors: Adrian Edralin and Darren Pak

# Files Submitted
* solutions.lc
* sort.lc

# Errors and Other Notes
* No known errors.

# Critical Appraisal
* Write the functions in pseudo code that is independent of the particular programming language used to implement the algorithm.
- isempty
    - if the list is an empty list return true otherwise return false

- fib
    - n in this case refers to the input
    - if n is 0 return 0 if it is 1 return 1
    - else return fib of n - 1 + fib of n - 2

- length
    - if the list is empty return 0
    - else remove the head of the list, add 1, and return the tail of the list

- even
    - if the list is empty return true
    - if the list has 1 element return false
    - else remove the first two elements and try again

- append
    - if list is empty add element to list as head
    - else add element to the end of the list as tail
   
- reverse
    - if list is empty return the empty list
    - else remove the head of the list and append it to move it to the end
    - this allows all elements to be moved to end resulting in a reversed list

- weave 
    - if list 2 is empty then return empty list
    - compare the first elements of each list
    - 

- insert
    - if list is empty, return  list with element in list
    - else compare the element to the head of the list
        - if the head is greater than then put element at the front of the list
        - if the head is less than the element, move the head to towards the front of the list and return with the tail of the list

- sort
    - if the list is empty return an empty list
    - else insert the head and sort the tail of the list again
    - this will craete a pattern until all of the elements have been removed from the list then inserted back into the list to have a perfectly sorted list
    - represents insertion sort

- merge
    - essentially this is the same as weave from our last assignment
    - remove the heads from both the lists
    - compare which head of the list is lesser
    - put the lesser head at the front and continue removing/comparing head until one of the lists is out of elements
    - append the remaining list to the resulting list to have a merged list

- mergesort
    - sort the elements of the list into even indexes and odd indexes to split the list into two
        - this is done by removing elements from the list and adding them to holder lists alternatively until there are no more elements
        - merge the results of sorting the odds and evens
            - this will split the odds and evens again until each element is in its own list
            - from there, the algorithm will merge individual elements into lists maintaining order until a sorted list is resulted

- print
    - translates the syntax into list form (ex. (cons 1 (cons 2 (cons 3 nil))) translates to [1,2,3])
    - if the list is empty it will return []

* Make drawings that contain pictures of the heap illustrating your algorithms.
- Drawings can be found here: https://github.com/dapak2002/Pak-D-CPSC-354-Report/blob/main/Assignment3/LambdaFun/pictures/Algorithm-Drawings.pdf

* Document known bugs (if any).
No known bugs or errors.

* Make some interesting observations about this assignment. For example, show me the most interesting work you did on testing and debugging. Maybe inspecting the heap while debugging made you think of garbage collection, a topic that would be worth exploring.
- Something interesting that we observed in this assignment was the difficulty of implementing mergesort in our lambda calculus. It was difficult to split the lists without any pre-defined functions, and due to haskell logic we were also unable to store any defined variables. In order to work around this we created even and odd lists and used a pointer to keep track of elements so the lists get sorted.
- Another interesting observation we found out was that the print function was using the addresses of the data in a list to print out that data, rather than just accessing the data itself. We found this out because we kept receiving the error "trying to access a non-address".
- Again as noted in previous assignments, Lambda Calculus makes learning and using recursive functions much simpler than other programming languages. In this case, since it is substitution, it is easier to keep track of what is the expected outcome in debugging and loops. However, it can also be it's own weakness due to how little control you have over how far the loop goes or keeping track of objects across recursion
    - In our case, we had to come up with our own method of controlling the loop and how far to go with each calculation and by using special cases like empty lists, we are able to define when the end of a loop is
    - Another interesting note is that infinite runtimes are a far more common bug/error with this type of programming due to the nature of the language
        - Other common bugs we ran into were syntax errors (learning how to use semi-colons vs. commas vs. new lines) and since we are creating our own syntax, not every function you would typically find in a programming language is available so we have to create our own "helper" functions to accomplish this
