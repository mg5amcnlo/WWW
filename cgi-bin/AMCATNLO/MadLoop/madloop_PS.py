#!/usr/bin/env python
import cgi
import sys
import os
import cgitb
import smtplib
cgitb.enable()


def sendemail(user, process, procdir):
  try:
    server = os.environ['MADGRAPH_SMTP']
  except KeyError:
    server = 'localhost'
    
  sender = 'amcatnlo@cern.ch'
  to = ["marco.zaro@gmail.com", "valentin.hirschi@gmail.com"]
  subject = 'MADLOOP: %s submitted the process %s ' %(user, process)
  text = 'The process has been labeled: %s .\n\
This message is automatically sent after each submission' %(procdir)

  message="""\
Subject: %s


      %s
      """ %(subject, text)

  server = smtplib.SMTP(server)
  server.sendmail(sender, to, message)
  server.quit()


def PrintErrorPage(msg):
  print "Content-Type: text/html\n\n"
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



form = cgi.FieldStorage()

procstring = form.getvalue('proc_string', 'wrong')
alphaew = str(form.getvalue('alphaew', ' 0'))
model = form.getvalue('model', ' ')

madloop_path = "/nfs/madgraph/AMCATNLO/MadLoop/"
html_path = "/nfs/madgraph/AMCATNLO/MadLoop/htmlresults/"

try:
  ialphaew=int(alphaew)
except ValueError:
  PrintErrorPage('QED power must be an integer.')
  sys.exit()

try:
  user = os.environ['REMOTE_USER']
except KeyError:
  user = 'defaultUSER'

try:
  host=os.environ['HTTP_HOST']
except KeyError:
  host="cp3wks05.fynu.ucl.ac.be"

part_dict = { 'd':1, 'u':2, 's':3, 'c':4, 'b':5, 't':6, \
              'd~':-1, 'u~':-2, 's~':-3, 'c~':-4, 'b~':-5, 't~':-6, \
              'e-':11, 've':12, 'mu-':13, 'vm':14, 'ta-':15, 'vt':16, \
              'e+':-11, 've~':-12, 'mu+':-13, 'vm~':-14, 'ta+':-15, 'vt~':-16,\
              'g':21, 'a':22, 'z':23, 'w+':24, 'h':25, 'w-':-24, '>':'->' }

#check proc string
proc_list = procstring.lower().split()

try:
  ninitial = proc_list.index('>')
except ValueError:
  PrintErrorPage('No initial-final state separator character ( > ) found in the submitted process.')
  sys.exit()

if ninitial>2:
  PrintErrorPage('Too many initial-state particles.')
  sys.exit()

pdglist = []

for part in proc_list:
  try:
    pdglist.append(str(part_dict[part]))
  except KeyError:
    PrintErrorPage('%s is not a valid particle.' % part)
    sys.exit()
  
pdg_string = "  ".join(p for p in pdglist)


nlight={'smNLO':4, 'smzerobNLO':5}[model]

if host != "cp3wks05.fynu.ucl.ac.be":
  #not on the testing machine:
  ipath = 1
  proc_path = "PROC_ML_%d" % ipath
  while os.path.exists(os.path.join(madloop_path,"Processes", proc_path)):
    ipath += 1
    proc_path = "PROC_ML_%d" % ipath
  html_name = proc_path + ".htm"
  theprocess = "%s, in the %d light flavours SM, QED power %s" \
      % (procstring.lower(), nlight, alphaew)
  os.chdir(madloop_path)
  os.system("sed  -e s/XXPROCSTRINGXX/\"%s\"/g -e s/XXSTATUSXX/\"%s\"/g %s > %s " \
      % (theprocess, "Generating code (this may take some time)...", \
        os.path.join(html_path,"template_html_page.htm"),\
        os.path.join(html_path,html_name)) )
  os.system("chmod o+r %s" % os.path.join(html_path,html_name))
    
    
  print "Content-Type: text/html\n\n\
  <html><body> \n\
  <meta HTTP-EQUIV=\"REFRESH\" CONTENT=\"1; URL=http://%s/AMCATNLO/AMCATNLOdata/MadLoop/htmlresults/%s\"\n\
  </body></html>\n" % (host, html_name)
  sendemail(user, procstring, proc_path)
  os.system("echo PROCESS SUBMITTED BY %s > LOGS/%s.log " % (user, proc_path) )
  os.system("./NLOComp.sh %s %s \"%s\" %s  >> LOGS/%s.log 2>&1 &"% (proc_path, model, pdg_string, alphaew, proc_path))
else:
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
  pass

sys.exit()



