from rpy2.robjects import r as R

from check import start_log, check, run, save_grades

lab = "ex4"
language = "r"
possible = 6
record = True  # turn on when ready to record grades
g = globals()

score = start_log(lab, 0, possible, g, record, language)

r_source = R['source']
try:
    m = r_source(lab+'.R')
except:
    pass

run("R('testInput       = factor(c(11,0,40,-1))')", g) 
run("R('correctOutput   = 50')", g) 
run("R('functionOutput = sumFactor(testInput)')", g)
score += check("R('all.equal(correctOutput, functionOutput)')[0]", True, 3, g)

run("R('testInput       = factor(c(1,0,2,3,2,1,1,1))')", g) 
run("R('correctOutput   = 11')", g) 
run("R('functionOutput = sumFactor(testInput)')", g)
score += check("R('all.equal(correctOutput, functionOutput)')[0]", True, 3, g)

save_grades(lab, score, possible, record)

