# --------------------------------------------------------------
# LAB 8 (R Arrays and Functions)
# --------------------------------------------------------------

# Complete the function bodies as indicated by the comments under the
# function definition.  You should try to pass all the provided tests
# to ensure that your implementation conforms to the requirements
# as much as possible.  Before pushing to github, make sure that
# your script is a **syntactically valid** R script.

# To run the tests, type in the terminal "Rscript lab8.R" and press enter.

# Rscript is a version of the R interpreter that allows you
# to execute the R code contained in a given file 
# (Python analog:  "python lab5.py")

# The tests for this lab use the R suite for unit tests 
# RUnit, which we import using the R way to load packages:

library(RUnit)
errmsg = function(err) print(paste("ERROR:  ",err))

# The test will be indicated after the function body you'll need
# to implement.

# You can determine if you have a syntactically **invalid**
# script by running it:
#  $ Rscript lab8.R
#  Loading required package: methods
#  <snip>
#  Execution halted
# If the script terminates with "Execution halted" then you need
# to modify your script to ensure that it is valid code.

# Good luck!


# --------------------------------------------------------------
# EXERCISE 1 
# --------------------------------------------------------------

simulateGradeBook  = function(mStudents, nItems, scoreRange=c(0,100)){
    # Implement this function body so that it returns 
    # a matrix whose row i contains randomly simulated grades for a given student
    # at nItems exams. Each row corresponds to the grades a unique student 
    # in a class of nStudents (i.e. the returned matrix should be a 
    # nStudents x nItems matrix). 
    # Moreover, the simulated grades should satisfy the following constrains:
    # (1) No grade should be below scoreRange[1] nor above scoreRange[2]
    # (2) The class grades for each grade items (i.e. each column) should be
    # normally distributed with mean 50 and variance 10 
    # (3) The returned grades should not have any decimal part

    grades = replicate(nItems, rnorm(mStudents, mean=50, sd=10))
    minG = scoreRange[1]; maxG = scoreRange[2]
    grades[ grades > maxG ] = maxG
    grades[ grades < minG ] = minG
    return(round(grades))
}

tryCatch(
    checkEquals(dim(simulateGradeBook(2,4)), c(2,4)),
    error = function(err) errmsg(err)
)
set.seed(1)
tryCatch(
    checkEquals(simulateGradeBook(2,4),
                matrix(c(44,42,53,55,52,66,42,57),nrow=2,byrow=TRUE)),
    error = function(err) errmsg(err)
)
set.seed(1)
tryCatch(
    checkEquals(simulateGradeBook(2,3,scoreRange=c(40,60)),
                matrix(c(44,42,53,52,60,42),nrow=2,byrow=TRUE)),
    error = function(err) errmsg(err)
)
set.seed(1)
tryCatch(
    checkEquals(simulateGradeBook(4,2),
                matrix(c(44, 52, 42, 66, 53, 42, 55, 57), nrow=4)),
    error = function(err) errmsg(err)
)
set.seed(1)
tryCatch(
    checkEquals(simulateGradeBook(5,1,scoreRange=c(45,55)),
                matrix(c(45, 52, 45, 55, 53), nrow=5)),
    error = function(err) errmsg(err)
)


# --------------------------------------------------------------
# EXERCISE 2 
# --------------------------------------------------------------

solveLinEq  = function(A, b) {
    # Implement this function body so that it returns
    # (1) the vector x that solves the linear equation 
    #     Ax = b  when A is an invertible matrix
    # (2) the string "The matrix is not invertible: too tough for me!"
    #       when A a square matrix but is not invertible (use warning2!)
    # (3) the string "I don't know how to solve for matrices with m rows and n columns"
    #     where m and n should be substituted by their true values in the string above (use warning1)
    #       when A is not square
    # (4) the string "The vector has length k, but the matrix is a m x m matrix: impossible problem!"
    #     where k and m should be replaced by their actual values
    #       when the dimension of A and the length of b are incompatible (use warning3!)

    warning1 = "I don't know how to solve for matrices with %d rows and %d columns"
    warning2 = "The matrix is not invertible: too tough for me!"
    warning3 = "The vector has length %d, but the matrix is a %d x %d matrix: impossible problem!"
    m = dim(A)[1]; n = dim(A)[2]; k = length(b)
    if(m != n){
        return(sprintf(warning1, m, n))
    }
    if(det(A) == 0){
        return(warning2)
    }
    if(n != k){
        return(sprintf(warning3, k,  m, n))
    }
    return(solve(A)%*% matrix(b, ncol=1))
}

# Tests:
A = diag(2)
b = c(1,0)
tryCatch ( 
    checkEquals(solveLinEq(A,b), matrix(b)),
    error = function(err) errmsg(err)
)
A = matrix(c(1,2,3,4), nrow=2)
b = c(1,5)
tryCatch ( 
    checkEquals(solveLinEq(A,b), matrix(c(5.5, -1.5))),
    error = function(err) errmsg(err)
)
A = matrix(c(1,0,0,0), nrow=2)
tryCatch ( 
    checkEquals(solveLinEq(A,b), "The matrix is not invertible: too tough for me!"),
    error = function(err) errmsg(err)
)
A = diag(3)
tryCatch ( 
    checkEquals(solveLinEq(A,b), "The vector has length 2, but the matrix is a 3 x 3 matrix: impossible problem!"),
    error = function(err) errmsg(err)
)
A = matrix(c(1,0,0,0,0,0), nrow=2)
tryCatch ( 
    checkEquals(solveLinEq(A,b), "I don't know how to solve for matrices with 2 rows and 3 columns"),
    error = function(err) errmsg(err)
)

