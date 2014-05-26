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
#  (2) an attribute named 'thresholds', which is a numeric vector whose
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

spotTooManyNA  = function(df, naThreshold, axis){
    naProp  = apply(is.na(df), axis, mean)
    indices = which(naProp >= naThreshold)
    names(indices) = NULL
    return(indices)
} 

outlierCutOff = function(dataVec){
    Q = quantile(dataVec, probs = c(0.25, 0.75), na.rm = T, names = F)
    IQR = diff(Q)
    cutoff = c(down=Q[1] - 1.5*IQR, up=Q[2] + 1.5*IQR)
    return(cutoff)
}

spotOutliers = function(df){
    outliers = list()
    for(j in 1:ncol(df)){
        cutoff = outlierCutOff(df[[j]])
        for(i in 1:nrow(df)){
            if(!is.na(df[i,j]) && 
               (df[i,j] < cutoff['down'] || cutoff['up'] < df[i,j])){
                outliers = c(outliers, list(c(i,j)))
            }
        }
    }
    return(outliers)
}


dfDiagnosis  = function(data, naRow, naCol){
   
   cleanData = data

   badRows   = spotTooManyNA(data, naRow, axis=1)
   if(length(badRows)) cleanData = data[-badRows,]

   badCols   = spotTooManyNA(cleanData, naCol, axis=2)
   if(length(badCols)) cleanData = cleanData[, -badCols]

   outliers  = spotOutliers(cleanData)
   if(length(outliers)){
      rowsWithOutliers = sapply(outliers, function(outlier) outlier[1])
      cleanData = cleanData[-rowsWithOutliers,]
   }
               
   diag = list(rawData=data, thresholds=c(naRow, naCol),
               badRows=badRows, badCols=badCols,
               outliers=outliers, cleanData=cleanData)

   class(diag) = 'dfDiagnosis'
   return(diag)

}

# --------------------------------------------------------------
# TEST 5 
# --------------------------------------------------------------

source('test.R')

x1 = c(10000, 1:999)
x2 = c(1:900, rep(NA, 100))
x3 = c(1:800, rep(NA, 200))
y  = c(1:999, NA) 

naR = 0.5; naC = 0.05

data = data.frame(X1=x1, X2=x2, X3=x3, X4=y, X5=y, X6=y, X7=y, X8=y, X9=y, X10=y)
cleanData = data[-c(1,1000), -c(2,3)]
dfDiag = dfDiagnosis(data, naRow=naR, naCol=naC)


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
         checkEquals(dfDiag$thresholds, c(naR,naC)),
	 error = function(err) errmsg(err)
)

tryCatch(
         checkEquals(dfDiag$badRows, 1000),
	 error = function(err) errmsg(err)
)


tryCatch(
         checkEquals(dfDiag$badCols, c(2,3)),
	 error = function(err) errmsg(err)
)

tryCatch(
         checkEquals(dfDiag$outliers, list(c(1,1))),
	 error = function(err) errmsg(err)
)

tryCatch(
         checkEquals(dfDiag$cleanData, cleanData),
	 error = function(err) errmsg(err)
)
