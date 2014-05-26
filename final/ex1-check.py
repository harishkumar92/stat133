from rpy2.robjects import r as R

from check import start_log, check, run, save_grades

lab = "ex1"
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


run("R('testInput = c(\"023-45-7890\") ')", g) 
run("R('correctOutput = c(\"023-45-7890\") ')", g) 
run("R('functionOutput  = extractSecuNum(testInput)')", g)
score += check("R('all.equal(correctOutput, functionOutput)')[0]", True, 3, g)
run("R('testInput = c( \"For example\", \"023-45-7890\", \"would be a valid value\", \"but 05-09-1995\", \"and 059-2-27 would not be.\", \"Also 011-99-2234 is okay.\") ')", g) 
run("R('correctOutput = c( \"023-45-7890\", \"011-99-2234\") ')", g) 
run("R('functionOutput  = extractSecuNum(testInput)')", g)
score += check("R('all.equal(correctOutput, functionOutput)')[0]", True, 3, g)

save_grades(lab, score, possible, record)

