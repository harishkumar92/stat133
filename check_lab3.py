import json
import logging
import os

lab = "lab3"
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

score += check('os.path.isfile("'+lab+'.py")', True, 2)
student['grades'][lab] = {'earned': score, 'possible': possible}

try:
    m = __import__(lab)
except:
    pass

score += check('m.reverse("ATGATT")', 'TTAGTA', 2)
score += check('m.is_dna("ANT")', False, 2)
score += check('m.is_dna("ATT")', True, 2)
score += check('m.is_rna("AU")', True, 2)
score += check('m.is_rna("AT")', False, 2)
score += check('m.reverse("ATC")', 'CTA', 2)
score += check('m.transcribe("ATCGTC")', 'AUCGUC', 2)
score += check('m.complement("ATCGGA")', "TAGCCT", 2)
score += check('m.is_complement("TAGCCT", "ATCGGA")', True, 2)
score += check('m.reversecomplement("ATGCAA")', 'TTGCAT', 2)
score += check('m.gc_content("ACAAGGGCT")', 0.5555555555555556, 4)
score += check('m.get_codons("ATGCCC")', ['ATG', 'CCC'], 4)

student['grades'][lab] = {'earned': score, 'possible': possible}
logging.info("You got a %s out of %s.", str(score), str(possible))
save_grades(student, 'grades.json')
