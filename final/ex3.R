# --------------------------------------------------------------
# EXERCISE 3 
# --------------------------------------------------------------


#  Implement the body of the function 'keepDoubleQuotesOnly' below so that
#  it returns a character vector containing only the elements 
#  of the character vector 'theString' (passed as a function argument)
#  that have more than one double quoted string.
#  
#  For example, if the following strings 
#  
#  this "line" has "quoted" "strings"
#  "one" more to follow "two"
#  "a""b"
#  there's "one" quoted string
#  Here's a quote "
#  No quotes here
#  
#  are the elements of the character vector 'theString',
#  then the function should return a character vector 
#  with the three first strings only. 



keepDoubleQuotesOnly = function(theString){
# Write your code here
  return (theString[grep('".*".*".*"', theString, perl=TRUE)])

}

# --------------------------------------------------------------
# TEST 3 
# --------------------------------------------------------------

source('test.R')

testInput  = c('this "line" has "quoted" "strings"',
               'there\'s "one" quoted string',
               '"one" more to follow "two"',
               'Here\'s a quote "',
               '"a""b"')


correctOutput = c('this "line" has "quoted" "strings"',
                  '"one" more to follow "two"',
                  '"a""b"')

functionOutput = keepDoubleQuotesOnly(testInput) 

tryCatch(
         checkEquals(correctOutput, functionOutput),
	 error = function(err) errmsg(err)
	 )


