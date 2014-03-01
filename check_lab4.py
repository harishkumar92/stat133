import json 
import logging
import os

lab = "lab4"
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
score += check('os.path.isfile("'+lab+'.py")', True, 8)
student['grades'][lab] = {'earned': score, 'possible': possible}

try:
    m = __import__("lab4")
except:
    pass

rev = m.Cipher("reverse")
logging.info('Executed rev = m.Cipher("reverse")')
score += check('rev.encrypt("testcase")', "gvhgxzhv", 2)
score += check('rev.decrypt("testcase")', "gvhgxzhv", 2)
score += check('rev.isvalid("testcase")', True, 2)
score += check('rev.isvalid("123")', False, 2)
score += check('rev.isvalid("TestCase")', False, 2)

rot = m.Cipher("rot13")
logging.info('Executed rot = m.Cipher("rot13")')
score += check('rot.encrypt("testcase")', "grfgpnfr", 2)
score += check('rot.decrypt("testcase")', "grfgpnfr", 2)
score += check('rot.isvalid("testcase")', True, 2)

hyb = m.Cipher("hybrid")
logging.info('Executed hyb = m.Cipher("hybrid")')
score += check('hyb.encrypt("testcase")', "ujvulnvj", 2)
score += check('hyb.decrypt("testcase")', "ujvulnvj", 2)
score += check('hyb.isvalid("testcase")', True, 2)

student['grades'][lab] = {'earned': score, 'possible': possible}
logging.info("You got a %s out of 30.", str(score))
save_grades(student, 'grades.json')
