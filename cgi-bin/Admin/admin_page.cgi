#!/usr/bin/perl -w

use Date::Manip;

$server  = $ENV{'SERVER_NAME'};
$dirMAD  = $ENV{'MADGRAPH_DATA'};
$dirBASE  = $ENV{'MADGRAPH_BASE'};

print "Content-type: text/html\n\n"; 
print   "<HTML> <HEAD> \n";
print  "<META HTTP-EQUIV=\"Pragma\" CONTENT=\"no-cache\" > ";
print  "\n <TITLE>Administration page for the Data Base</TITLE></HEAD> \n <BODY>";

  print "<H1 align=center>Cluster Administration Page</H1><BR><BR><BR>\n";

  print "<a href=\"\/cgi-bin\/Admin\/admin_db.cgi\"><H2 align=center>Display users database</H2></a>\n";

# Write header
#  print "<a href=\"http://cp3wks05.fynu.ucl.ac.be/cgi-bin/ViewVC/viewvc.cgi\"><H2 align=center>Go to the CVS Web Site </H2></a>\n";

# Write header
  print "<a href=\"http://cp3.phys.ucl.ac.be/dokuwiki/doku.php?id=wg_mad:madgraph\"><H2 align=center>Go to the development team Wiki</H2></a></td>\n";

# Write header
  print  "<a href=\"http:\/\/$server\/cgi-bin\/Admin\/update.cgi\"><H2 align=center>Run the update script [note this script auto-run every hour (every h30)</H2>\n";
# Write header
  print  "<a href=\"http:\/\/$server\/cgi-bin\/Admin\/script.cgi\"><H2 align=center>Run bzr based script</H2>\n";
# Write header
  print  "<a href=\"http:\/\/$server\/package_info.dat\"><H2 align=center>check package status</H2>\n";
  print  "<a href=\"http:\/\/$server\/models_db.dat\"><H2 align=center>check model db</H2>\n";
  print  "<a href=\"http:\/\/$server\/auto_update.log\"><H2 align=center>check log of last auto-update</H2>\n";


  print  "\</BODY> \n";
  print  "\</HTML> \n";
