# --------------------------------------------------------------
# EXERCISE 5
# --------------------------------------------------------------


#  Implement below the constructor of a class called 'dfDiagnosis' whose aim 
#  is to facilitate the cleaning of numeric data frames.
#  In particular, this class should help spot outlier values in columns,
#  as well as rows and columns having a proportion of NA 
#  value higher than certain specified thresholds.
#  
#  The class constructor below takes a numeric data frame (numeric data frame = the
#  data frame columns are numeric vectors) as its 'data' argument, as
#  well as two other arguments, 'naRow' and 'naCol', specifying
#  the proportions of NA values beyond which rows and columns, respectively,
#  are considered bad, and should be removed from the data frame. 
#  
#  The constructor returns a 'dfDiagnosis' object that has the 
#  following attributes:
#  
#  (1) an attribute named 'rawData' that contains the original data frame
#  
#  (2) an attribute named 'theesholds', which is a numeric vector whose
#  first element is 'naRow', and whose second element is 'naCol'
#  
#  (3) an attribute named 'badRows', which is a numeric vector 
#  containing the indices of the rows whose proportion 
#  of NA values is higher than the threshold specified by 'naRow'
#  
#  (4) an attribute named 'badCols', which is a numeric vector 
#  containing the indices of the columns whose NA value proportion 
#  is higher than 'naCol'
#  
#  (5) an attribute named 'outliers', which is a list of 
#  pairs of indices (i,j) (represented as numeric vectors with
#  two elements) indicating the matrix coordinates of potential outliers
#
#  (6) an attribute named 'cleanData' containing a clean data frame where
#  the rows and columns beyond threshold in data have been removed, along 
#  with the rows containing at least one outlier




dfDiagnosis  = function(data, naRow, naCol){
  nrows = dim(data)[1] ; ncols = dim(data)[2]

  
  #get bad rows
  rowsums = apply(data, 1, function(x) sum(is.na(x)))
  narowprops = rowsums/nrows
  narowprops = narowprops > naRow
  badRows = which(narowprops == TRUE)
  attributes(badRows) = NULL
  
  #get bad columns
  colsums = apply(data, 2, function(x) sum(is.na(x)))
  nacolprops = colsums/ncols
  nacolprops = nacolprops > naCol
  badCols = which(nacolprops == TRUE)
  attributes(badCols) = NULL
   # Write your code here!
  
  #get outliers
  #compute [upper, lower] limits for each column first
  upper = apply(data, 2, getUpper) #upper[i] -> upperlimit for col i
  lower = apply(data, 2, getLower)
  
  outliers = list()
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      if (is.na(data[i,j]) == FALSE) {
        if ((data[i, j] > upper[j]) || (data[i,j] < lower[j])) {
          outlier_coord = list(c(i,j))
          outliers = c(outliers, outlier_coord)
        } 
      }
    }
  }
  
  outlying_rows = sapply(outliers, function(x) x[[1]][1])
  removed_rows = c(outlying_rows, badRows)
  cleanData = data[-removed_rows,]
  cleanData= cleanData[,-badCols]
  newnrows = dim(cleanData)[1]
  rownames(cleanData) = seq(1:newnrows)
  
  object = list(rawData = data, thresholds=c(naRow, naCol), 
                badRows=badRows, badCols=badCols, outliers=outliers,
                cleanData = cleanData)
  class(object) = 'dfDiagnosis'
  
  return (object)
  
  


}

#Helper
getUpper = function(column) {
  temp = summary(column)
  Q3 = temp[5] ; Q1 = temp[2] ; median = temp[3]
  IQR  = Q3 - Q1
  return (median + (1.5*IQR)) 
}



getLower = function(row) {
  temp = summary(row)
  Q3 = temp[5] ; Q1 = temp[2] ; median = temp[3]
  IQR  = Q3 - Q1
  return (median - (1.5*IQR)) 
}

# --------------------------------------------------------------
# TEST 5 
# --------------------------------------------------------------
source('test.R')

data = data.frame(X1=c(NA,NA,NA,100), 
		  X2=c(1,NA,1,100), 
		  X3=c(1,NA,1,100), 
		  X4=c(100,NA,0,1))

dfDiag = dfDiagnosis(data, naRow=0.5, naCol=0.6)


tryCatch(
         checkEquals(class(dfDiag), 'dfDiagnosis'),
	 error = function(err) errmsg(err)
)

tryCatch(
         checkEquals(length(dfDiag), 6),
	 error = function(err) errmsg(err)
)

tryCatch(
         checkEquals(dfDiag$rawData, data),
	 error = function(err) errmsg(err)
)


tryCatch(
         checkEquals(dfDiag$thresholds, c(0.5,0.6)),
	 error = function(err) errmsg(err)
)

tryCatch(
         checkEquals(dfDiag$badRows, 2),
	 error = function(err) errmsg(err)
)


tryCatch(
         checkEquals(dfDiag$badCols, 1),
	 error = function(err) errmsg(err)
)

tryCatch(
         checkEquals(dfDiag$outliers, list(c(1,4), c(4,2), c(4,3))),
	 error = function(err) errmsg(err)
)

tryCatch(
         checkEquals(dfDiag$cleanData, data.frame(X2=1, X3=1, X4=0)),
	 error = function(err) errmsg(err)
)
