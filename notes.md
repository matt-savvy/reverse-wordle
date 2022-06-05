## representing feedback

- List (Char, CharFeedback)

- Array (Char, CharFeedback)



## getfeedback invariants

- for each letter in the solution set,

   - the number of letters that are Correct or InWord should be at most equal
     to the number of times that letter shows up in the solution

- for each letter in the feedback that is listed as NotInWord, that letter should not be a member
of the solution list

## alg

REVOR
EVARG

solution [E, 1, V, 1, A, 1, R, 1, G 1]

go over each letter in guess
    does it match that letter in the solution?
        Correct
        reduce the soltuion dict for that key by 1
    is the letter contained in the solution dict?
        InWord
        reduce the soltuion dict for that key by 1
    else
        NotInWord

















