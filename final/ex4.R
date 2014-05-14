# --------------------------------------------------------------
# EXERCISE 4
# --------------------------------------------------------------

#  Implement the body of 'sumFactor' so that it takes a factor
#  whose levels are numerics and returns the sum of the levels.
#  
#  For instance, when the following factor 
#  
#  > a
#    [1] 1 0 2 3 2 1 1 1
#   Levels: 0 1 2 3
#   
#  is passed to 'sumFactor', the function should return 11



sumFactor  = function(aFactor){
# Write your code here!
  return (sum(as.numeric(levels(testInput)[testInput])))

}


# --------------------------------------------------------------
# TEST 4 
# --------------------------------------------------------------

source('test.R')

testInput       = factor(c(1,0,2,3,2,1,1,1))
correctOutput   = 11
functionOutput  = sumFactor(testInput) 

tryCatch(
         checkEquals(correctOutput, functionOutput),
	 error = function(err) errmsg(err)
)


