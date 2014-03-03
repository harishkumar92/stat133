import json
import logging
import os

from numpy import array
from pandas import DataFrame

lab = "lab5"
score = 0
possible = 30
logging.basicConfig(filename=lab+".log",
                    filemode='w',
                    level=logging.INFO)

def get_grades(filename):
    with open(filename) as infile:
        grades = json.load(infile)
    return grades

def save_grades(content, filename):
    with open(filename, 'w') as outfile:
        json.dump(content, outfile, sort_keys = True, indent = 4)

def correct(command, answer, score):
    logging.info('(%s points) %s is %s', score, command, answer)

def wrong(command, answer, result, score):
    logging.error('(%s points) Checking %s', score, command)
    logging.error('... Expecting: %s', answer)
    logging.error('... But got:   %s', result)

def check(command, answer, score):
    result = eval(command)
    if result == answer:
        correct(command, answer, score)
        return score
    else:
        wrong(command, answer, result, score)
        return 0

student = get_grades('grades.json')
score += check('os.path.isfile("'+lab+'.py")', True, 4)
student['grades'][lab] = {'earned': score, 'possible': possible}

try:
    m = __import__(lab)
except:
    pass

score += check('m.simulate_grades(2,[5,5,5]).shape == (2,3)', True, 2)
g = m.simulate_grades(10, [5, 20, 40])
logging.info('Executed g = simulate_grades(10, [5, 20, 40])')
score += check('g[:,0].max() < 5', True, 2)
score += check('g[:,1].max() < 20', True, 2)
score += check('g[:,2].max() < 40', True, 2)
score += check('m.simulate_grade_df(3,{"M":5,"HW":5}).shape == (3,2)', True, 2)

a = m.GradeBook(array([[5,5],[1,1]]),['22','34'],['F','M'],[10, 10])
logging.info('Executed a = m.GradeBook(array([[5,5],[1,1]]),["22","34"],["F","M"],[10, 10]')
score += check('type(a.raw_grades) == DataFrame', True, 2)
score += check('a.total_grades', None, 2)
score += check('a.raw_grades.shape == (2,2)', True, 2)
score += check('a.raw_grades.ix[0,0]', 5, 2)
score += check('a.max_scores[0]', 10, 2)
b = a.compute_total_grades([0.3, 0.7], 100)
score += check('len(b)', 8, 2)
score += check('a.total_grades["22"]', 50, 2)
score += check('a.total_grades["34"]', 10, 2)

student['grades'][lab] = {'earned': score, 'possible': possible}
logging.info("You got a %s out of %s.", str(score), str(possible))
save_grades(student, 'grades.json')