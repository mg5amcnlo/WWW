#!/usr/bin/env python
import cgi
import sys
import os


def PrintErrorPage(msg):
  print "Content-Type: text/html"
  print 
  print"\
<html><body> \n\
	<font  color=\"#AA0000\" face=\"Courier\"><br><br>\n\
	Error:<br><br><hr></font>\n\
        <font  color=\"#000000\" face=\"Courier\"><p style=\"line-height:150%%;\">\n\
<p>\n\
%s <br>\n\
Please try again\n\
</p>\n\
</body></html>\n\
" % msg
  sys.exit()
  return True

form = cgi.FieldStorage()
user = sys.argv[1]
procstring = sys.argv[2]
model = sys.argv[3]
try:
  alphaew = sys.argv[4]
except ValueError:
  PrintErrorPage('Please insert a value for QED power')
  
madloop_path = "/nfs/madgraph/AMCATNLO/MadLoop/"

host=os.environ['HTTP_HOST']

part_dict = { 'd':1, 'u':2, 's':3, 'c':4, 'b':5, 't':6, \
              'd~':-1, 'u~':-2, 's~':-3, 'c~':-4, 'b~':-5, 't~':-6, \
              'e-':11, 've':12, 'mu-':13, 'vm':14, 'ta-':15, 'vt':16, \
              'e+':-11, 've~':-12, 'mu+':-13, 'vm~':-14, 'ta+':-15, 'vt~':-16,\
              'g':21, 'a':22, 'z':23, 'w+':24, 'h':25, 'w-':-24, '>':'->' }

#check proc string
proc_list = procstring.lower().split(' ')

try:
  ninitial = proc_list.index('>')
except ValueError:
  PrintErrorPage('No initial-final state separator character ( > ) found in the submitted process.')

if ninitial>2:
  PrintErrorPage('Too many initial-state particles.')

pdglist = []

for part in proc_list:
  try:
    pdglist.append(str(part_dict[part]))
  except KeyError:
    PrintErrorPage('%s is not a valid particle.' % part)
pdg_string = "  ".join(p for p in pdglist)


print "Content-Type: text/html"
print 
print"\
<html><body> \n\
	<font  color=\"#AA0000\" face=\"Courier\"><br><br>\n\
	Process submitted:<br><br><hr></font>\n\
        <font  color=\"#000000\" face=\"Courier\"><p style=\"line-height:150%%;\">\n\
<p>\n\
User:    %s <br>\n\
Process: %s <br>\n\
Model:   %s <br>\n\
alphaew: %s <br>\n\
Thank you!\n\
</p>\n\
</body></html>\n\
" % (user,pdg_string, model, alphaew ) 


if host != "cp3wks05.fynu.ucl.ac.be":
  os.cd(madloop_path)
#  os.system("./NLOComp.sh %s %s \"%s\" %s  "% (user, model, pdg_string, aphaew))
else:
  print "on test machine, nothing will be done"
  pass

sys.exit()



#" % (os.environ['REMOTE_USER'], "  ".join(p for p in pdglist), model, str(alphaew) ) 


