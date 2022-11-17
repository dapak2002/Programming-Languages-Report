# Assignment 1
 * Authors: Darren Pak and Adrian Edralin (the-drain-at-chap)
 * NOTE: This assignment was completed using "extreme programming". Adrian and I were in the same room with one person coding and the other focused on ideation. There may be a lack of "merges" from either side due to this programming technique. All commits and changes were recorded on Github.

# Files Submitted

* arithmetic.hs
* Calculator.hs
* Interpreter.hs
* numbers.cf
* README.md

# Errors and Other Notes

* Equalities for QQ and II are unsure and may not be the intended functionality
* Created a subN fucntion for divN
* Created other type conversions

# Critical Appraisal Part 1

```
3 + 2:  
addP (T (T (I))) (T (I))  
	T (addP (T(I)) (T(I)))  
		T (T (addP (I) (T(I))))  
			T( T (T (T (I)))) = 5
			
3 * 2:
multP (T (T (I))) (T(I))
    addP ((multP (T (I))) (T (I))) (T (I))
        addP ((addP (multP (I) (T (I))) (T (I))) (T (I)))
            addP (addP (T (I)) (T (I))) (T (I))
                addP (T (addP (I) (T (I)))) (T (I))
                    addP (T (T (T (I)))) (T (I))
                        T (addP (T (T (I))) (T (I)))
                            T (T (addP (T (I)) (T (I))))
                                T (T (T (addP (I) (T (I)))))
                                    T( T (T (T (T (I))))) = 6
				
5 - 3:
subN (S (S (S (S (S O))))) (S (S (S O)))
    subN (S (S (S (S O)))) (S (S O))
        subN (S (S (S O))) (S (O))
         subN (S (S O)) (O)
            (S (S O)) = 2
	    
(1/2) * (3/4):
multQ (QQ (S O) (T (I))) (QQ (S (S (S O)) (T (T (T (I))))))
    QQ (multN (S O) (S (S (S O)))) (multP (T (I)) (T (T (T (I)))))
        QQ (addN (multN (O) (S (S (S O))))) (multP (T (I) (T (T (T (I))))))
            QQ (addN (O) (S (S (S O)))) (multP (T (I) (T (T (T (I))))))
                QQ (S (S (S O))) (multP (T (I)) (T (T (T (I)))))
                    QQ (S (S (S O))) (addP (multP (I) (T (T (T (I))))) (T (T (T (I)))))
                        QQ (S (S (S O))) (addP (T (T (T (I)))) (T (T (T (I)))))
                            QQ (S (S (S O))) (T (addP (T (T (I))) (T (T (T (I))))))
                                QQ (S (S (S O))) (T (T (addP (T (I)) (T (T (T (I)))))))
                                    QQ (S (S (S O))) (T (T (T (addP (T (I)) (T (T (T (I))))))))
                                        QQ (S (S (S O))) (T (T (T (T (T (T (T (I)))))))) = (3/8)
					
-(3-5) = (5 - 3):
negI (II (S (S (S O))) (S (S (S (S (S O))))))
    II (S (S (S (S (S O)))) (S (S (S O)))) = 2
```

# Critical Appraisal Part 2

* The order of operations we designed for our calculator were: Level 1 -> Absolute Value and Negative; Level 2 -> Exponents; Level 3 -> Division, Modulus, and Multiplication; Level 4 -> Addition and Subtration. The lower number of the level the higher the priority of the operation.
* One example we used to guide this design was the problem |-2| + 8 / 2 - 4^2 * -(6) = 102. In order to get the answer of we have to follow the correct order of operations. The first step is turning -2 to 2 because of absolute value. The second step is turning 6 to -6 because of negatives. The third step is turning 4 to 16 because of exponentation. At this point of the problem the equation now looks like: 2 + 8 / 2 - 16 * (-6). The fourth step is dividing 8 by 2 and receiving the number 4. In the fifth step, 16 is considered -16 because subtraction is a later operation, so -16 and -6 are multiplied which results in 96. The problem is now viewed as 2 + 4 + 96 and all that is left is addition. The sixth step is adding 4 and 96 which receives 100. The last step is adding 2 and 100 and you receive 102, which is our final answer. Changing the order of operations in any way will change the final answer of this problem. We used an actual calculator as reference to make sure the order of operations process and answers were identical. Modulus was not included in this problem, but it is at the same level as multiplication and division and would take place after absolute value, negative, and exponentation.
* Our grammar reflects these designs as each operation in our grammar is identical to the priority level described above. The only difference is that the higher number after Exp means higher priority instead of the opposite in the level system above. Our grammar: Exp4 -> Num; Exp3 -> Abs, Neg; Exp2 -> Expn; Exp1 -> Div, Mod, Multi; Exp -> Add, Sub

# What We Learned and Interested Us and Tips for Improvement

* Recursion is a powerful tool, with many different use cases
* TIP: Not all fucntions that were needed to solve arithmetic were given such as "subN" and other type conversions we created
* TIP: Intended fucntionality for equalities was also unclear about what we had to code
* Overall, was a difficult project, but lessons were learned about using recursion and it was cool to see the parser in use as well
