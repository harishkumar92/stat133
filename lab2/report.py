import json

infile = open('data/test.json', 'r')
student = json.load(infile)
infile.close()
# close infile

name = student['name']
grades = student['grades']


print "." * 70
for lab in sorted(grades.keys()):
    print name, " got a ",  grades[lab]['earned'], " out of ", grades[lab]['possible'], " on ", lab
    # print "\n"
	
print "." * 70