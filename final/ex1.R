# --------------------------------------------------------------
# EXERCISE 1
# --------------------------------------------------------------

#  Social security numbers in the United States are represented by 
#  numbers conforming to the following format:
#
#    a leading 0 followed by two digits
#    followed by a dash 
#    followed by two digits
#    followed by a dash
#    finally followed by four digits
#  
#  For example 023-45-7890 would be a valid value, 
#  but 05-09-1995 and 059-2-27 would not be.
#  
#  Implement the body of the function 'extractSecuNum' below so that it  
#  returns a numeric vector whose elements are Social Security numbers 
#  extracted from a text, i.e., a vector of strings representing the text lines,
#  passed to the function   as its 'text' argument. 
#  (You can assume that each string in 'text' contains 
#  either zero or one Social Security numbers.)


extractSecuNum  = function(text){
# Write your code here!
  m = regexpr("0[0-9]{2}-[0-9]{2}-[0-9]{4}", text)
  val = regmatches(testInput, m)
  return (val)

}


# --------------------------------------------------------------
# TEST 1 
# --------------------------------------------------------------
source('test.R')

testInput = c(
'For example',
'023-45-7890',
'would be a valid value', 
'but 05-09-1995',
'and 059-2-27 would not be.',
'Also 011-99-2234 is okay.'
)

correctOutput = c(
'023-45-7890',
'011-99-2234'
)

functionOutput  = extractSecuNum(testInput) 

tryCatch(
         checkEquals(correctOutput, functionOutput),
	 error = function(err) errmsg(err)
)


