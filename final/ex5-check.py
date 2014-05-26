from rpy2.robjects import r as R

from check import start_log, check, run, save_grades

lab = "ex5"
language = "r"
possible = 24
record = True  # turn on when ready to record grades
g = globals()

score = start_log(lab, 0, possible, g, record, language)

r_source = R['source']
try:
    m = r_source(lab+'.R')
except:
    pass


run("R('x1 = c(10000, 1:999)')", g) 
run("R('x2 = c(1:900, rep(NA, 100))')", g) 
run("R('x3 = c(1:800, rep(NA, 200))')", g) 
run("R('y  = c(1:999, NA)')", g) 
run("R('naR = 0.5; naC = 0.05')", g) 
run("R('data = data.frame(X1=x1, X2=x2, X3=x3, X4=y, X5=y, X6=y, X7=y, X8=y, X9=y, X10=y)')", g) 
run("R('cleanData = data[-c(1,1000), -c(2,3)]')", g) 
run("R('dfDiag = dfDiagnosis(data, naRow=naR, naCol=naC)')", g) 
score += check("R('all.equal(class(dfDiag), \\'dfDiagnosis\\')')[0]", True, 3, g)
score += check("R('all.equal(length(dfDiag), 6)')[0]", True, 3, g)
score += check("R('all.equal(dfDiag$rawData, data)')[0]", True, 3, g)
score += check("R('all.equal(dfDiag$thresholds, c(naR,naC))')[0]", True, 3, g)
score += check("R('all.equal(dfDiag$badRows, 1000)')[0]", True, 3, g)
score += check("R('all.equal(dfDiag$badCols, c(2,3))')[0]", True, 3, g)
score += check("R('all.equal(dfDiag$outliers, list(c(1,1)))')[0]", True, 3, g)
score += check("R('all.equal(dfDiag$cleanData, cleanData)')[0]", True, 3, g)

save_grades(lab, score, possible, record)

