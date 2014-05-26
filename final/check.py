import json 
import logging
import os
import traceback
from subprocess import check_output

class_grades = '../../../data/grades.json'
tmp_grades = 'tmp.json'
student_grades = '../grades.json'
score = 0

def start_log(lab, minimum, possible, g, record=False, language="python"):
    logging.basicConfig(filename=lab+".log",
                    filemode='w',
                    level=logging.INFO)
    if language == "python":
        filename = lab+'.py'
    elif language == "r":
        filename = lab+'.R'
    else:
        raise ValueError("language must be python or r")
    score = check('os.path.isfile("'+filename+'")', True, minimum, g)
    save_grades(lab, score, possible, record, verbose=False)
    return score


def get_grades(filename):
    with open(filename) as infile:
        grades = json.load(infile)
    return grades

def save_grades(lab, score, possible, record=False, verbose=True):
    for d in grades:
        if d['login']==student['login']:
            penalty = 0
            note = ''
            commit = check_output(['git', 'log', '-1', '--format="%H"']).strip()[1:-1]
            if lab in d['grades']:
                old = d['grades'][lab]
                if 'penalty' in old:
                    penalty = old['penalty']
                if 'note' in old:
                    note = old['note']
                score = max(score - penalty, old['earned'])
            d['grades'][lab] = {'earned': score,
                                'possible': possible,
                                'penalty': penalty,
                                'hash': commit,
                                'note': note }
    filename = class_grades if record else tmp_grades
    with open(filename, 'w') as outfile:
        json.dump(grades, outfile, sort_keys = True, indent = 4)
    if verbose:
        print student['login'] + ' got ' + str(score)
        logging.info("You got a %s out of %s.", str(score), str(possible))

def run(command, g):
    logging.info('Executed '+command)
    try:
        exec(command, g)
    except:
        logging.exception('Got exception on main handler')
        logging.exception(traceback.format_exc())

def correct(command, answer, score):
    logging.info('(%s points) %s is %s', score, command, answer)

def wrong(command, answer, result, score):
    logging.error('(%s points) Checking %s', score, command)
    logging.error('... Expecting: %s', answer)
    logging.error('... But got:   %s', result)

def check(command, answer, score, g):
    g['os'] = os
    try:
        result = eval(command, g)
    except:
        wrong(command, answer, 'Traceback error', score)
        logging.exception('Got exception:')
        logging.exception(traceback.format_exc())
        return 0
    if result == answer:
        correct(command, answer, score)
        return score
    else:
        wrong(command, answer, result, score)
        return 0

grades = get_grades(class_grades)
student = get_grades(student_grades)
