from rpy2.robjects import r as R

from check import start_log, check, run, save_grades

lab = "ex2"
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


run("R('testInput      = c(\" hello \")')", g) 
run("R('correctOutput  = c(\"hello\")')", g) 
run("R('functionOutput = rmMultipleBlanks(testInput)')", g)
score += check("R('all.equal(correctOutput, functionOutput)')[0]", True, 3, g)
run("R('testInput      = c(\" hello, world \", \"\\n \\tStat 133 \")')", g) 
run("R('correctOutput  = c(\"hello, world\", \"Stat 133\")')", g) 
run("R('functionOutput = rmMultipleBlanks(testInput)')", g)
score += check("R('all.equal(correctOutput, functionOutput)')[0]", True, 3, g)

save_grades(lab, score, possible, record)

