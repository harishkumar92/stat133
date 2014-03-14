# --------------------------------------------------------------
# LAB 7 (R vector manipulations)
# --------------------------------------------------------------

# Complete the function bodies as indicated by the comments under the
# function definition. Make sure you pass the tests before you push
# your lab to github.

# To run the tests, type in the terminal "Rscript lab7.R" and press enter.

# Rscript is a version of the R interpreter that allows you
# to execute the R code contained in a given file 
# (Python analog:  "python lab5.py")

# The grading of this lab will be automated using the R suite of test 
# RUnit, which we import using the R way to load packages:

library(RUnit)

# In this test, we will make use of the following function to 
# have a nicer output of the error when a test fails:

errmsg = function(err) print(paste("ERROR:  ",err))

# The test will be indicated after the function body you'll need
# to implement.

# Good luck with your first R lab!


# --------------------------------------------------------------
# EXERCISE 1 
# --------------------------------------------------------------

createVector1  = function(n) {
    # Implement this function body so that it returns
    # the vector (1,..., n-1, n, n-1, n-2, ..., 1), where n is 
    # a natural number (1,2,3,...) passed as the function argument.

    "NotImplemented"
}

tryCatch ( checkEquals(createVector1(3), c(1,2,3,2,1)), 
	   error = function(err) errmsg(err))

tryCatch ( checkEquals(createVector1(2), c(1,2,1)),
           error = function(err) errmsg(err))

tryCatch ( checkEquals(createVector1(1), c(1)), 
	   error = function(err) errmsg(err))


# --------------------------------------------------------------
# EXERCISE 2 
# --------------------------------------------------------------

createVector2  = function(a,b,c,k,l,m) {
    # Implement this function body so that it returns
    # the vector (a,a,...,a,b,b,...,b,c,c,...c), where a is
    # repeated k times, b is repeated l times, and c is repeated 
    # m times. 

    "NotImplemented"
}

# Tests:
tryCatch ( 
          checkEquals(
                      createVector2('x','y','z', 2, 3, 4),
	              c('x','x','y','y','y','z','z','z','z')
		     ), 
          error = function(err) errmsg(err)
         )
tryCatch ( 
          checkEquals(
                      createVector2('abc','1','2', 2, 1, 2),
	              c('abc','abc','1','2', '2')
		     ), 
          error = function(err) errmsg(err)
         )
tryCatch ( 
          checkEquals(
                      createVector2('a','121','2c', 1, 1, 1),
	              c('a','121','2c')
		     ), 
          error = function(err) errmsg(err)
         )
tryCatch ( 
          checkEquals(
                      createVector2('1','2','3', 0, 1, 1),
	              c('2','3')
		     ), 
          error = function(err) errmsg(err)
         )

# --------------------------------------------------------------
# EXERCISE 3 
# --------------------------------------------------------------

createVector3  = function(label, n) {
    # Implement this function body so that it returns
    # the character vector (label 1, label 2, ..., label n), where
    # label is a string and n is an integer.

    "NotImplemented"
}

# Tests:
tryCatch ( 
          checkEquals(
                      createVector3('student', 3),
	              c('student 1', 'student 2', 'student 3')
		     ), 
          error = function(err) errmsg(err)
         )

tryCatch ( 
          checkEquals(
                      createVector3('item', 2),
	              c('item 1', 'item 2')
		     ), 
          error = function(err) errmsg(err)
         )
tryCatch ( 
          checkEquals(
                      createVector3('item', 1),
	              c('item 1')
		     ), 
          error = function(err) errmsg(err)
         )
tryCatch ( 
          checkEquals(
                      createVector3('3', 1),
	              c('3 1')
		     ), 
          error = function(err) errmsg(err)
         )

# --------------------------------------------------------------
# EXERCISE 4 
# --------------------------------------------------------------

createVector4  = function(a, b, s) {
    # Implement this function body so that it returns
    # the numeric vector 
    # (exp(a)cos(a), exp(a+s)cos(a+s), exp(a+2s)cos(a+2s),...,exp(a+ns)cos(a+ns))
    #    where a < b, a+ns <= b, and a+(n+1)s > b
    
    "NotImplemented"
}

# Tests:
tryCatch ( 
          checkEquals(
                      createVector4(3,4,1),
		      c(exp(3)*cos(3), exp(4)*cos(4))
		     ), 
          error = function(err) errmsg(err)
         )
tryCatch ( 
          checkEquals(
                      createVector4(1,5,3),
		      c(exp(1)*cos(1), exp(4)*cos(4))
		     ), 
          error = function(err) errmsg(err)
         )
tryCatch (
          checkEquals(
                      createVector4(1,5,2),
                      c(exp(1)*cos(1), exp(3)*cos(3), exp(5)*cos(5))
                     ),
          error = function(err) errmsg(err)
         )
tryCatch (
          checkEquals(
                      createVector4(3,5,3),
                      c(exp(3)*cos(3))
                     ),
          error = function(err) errmsg(err)
         )

