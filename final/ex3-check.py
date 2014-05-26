from rpy2.robjects import r as R

from check import start_log, check, run, save_grades

lab = "ex3"
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


run("R('testInput  = c(\\'\"line\" \"t\"\\')')", g) 
run("R('correctOutput = c(\\'\"line\" \"t\"\\')')", g) 
run("R('functionOutput = keepDoubleQuotesOnly(testInput)')", g)
score += check("R('all.equal(correctOutput, functionOutput)')[0]", True, 3, g)

run("R('testInput  = c(\\'this \"line\" has \"quoted\" \"strings\"\\', \\'theres \"one\" quoted string\\', \\'\"one\" more to follow \"two\"\\', \\'Heres a quote \"\\', \\'\"a\"\"b\"\\')')", g) 
run("R('correctOutput = c(\\'this \"line\" has \"quoted\" \"strings\"\\', \\'\"one\" more to follow \"two\"\\', \\'\"a\"\"b\"\\')')", g) 
run("R('functionOutput = keepDoubleQuotesOnly(testInput)')", g)
score += check("R('all.equal(correctOutput, functionOutput)')[0]", True, 3, g)

save_grades(lab, score, possible, record)

