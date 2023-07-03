#!/usr/bin/perl -w

#------------------------------------------------------------
# set variables for the most common path names
#------------------------------------------------------------



$dirBASE= $ENV{'MADGRAPH_BASE'};
$USER = $ENV{'USER'};
my $USER = $ENV{LOGNAME} || $ENV{USER} || getpwuid($<);
$| = 1;
print "Content-type: text/html\n\n";
print  "<HTML> <HEAD> \n";
print "<META URL=$ENV{'HTTP_REFERER'}\"> \n";
print  "</HEAD> <BODY> <CENTER>\n";

if (-e "$dirBASE/update.sh"){
    print "An update script has been found \n";
}
else {
    print "No update script found! Please create one called $dirBASE/update.sh!\n";
    print "</CENTER></BODY></HTML> \n \n";
    exit(0);
}

   print "and has been started, please wait while it is running...<br>\n";
   print "THIS MAY TAKE SEVERAL MINUTES, PLEASE DO NOT CLOSE OR STOP/REFRESH THIS WINDOW!!!</center><pre>\n";
   print "INFO: USER is $USER\n";
#uiuc can't make sud and it's username is sudo. while it's empty for UCL
if ( $USER ne "madgraph")   
{   
   system "cd $dirBASE; sudo -u madgraph $dirBASE/update.sh 2>&1";
}
else
{
	system "cd $dirBASE; $dirBASE/update.sh 2>&1";
}
print "</BODY></HTML> \n \n";
