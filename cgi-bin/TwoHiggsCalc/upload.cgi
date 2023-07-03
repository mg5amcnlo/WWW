#!/usr/bin/perl

#############################################################
#                                                          ##
#                   TwoMadGraph Project                    ##
#                                                          ##
# FILE : upload.cgi                                        ##
# VERSION : 1.1                                            ##
# DATE : 8 March 2006                                      ##
# AUTHOR : M. Herquet (UCL-CP3)                            ##
#                                                          ##
# DESCRIPTION : script to upload a file and run            ##
#   TwoHiggsCalc on it                                     ##
#############################################################

# Use CGI stuff
use CGI;

$uname  = $ENV{REMOTE_USER};
$dirMAD = $ENV{'MADGRAPH_DATA'};
$server = $ENV{'SERVER_NAME'};
$dirHTML= "MadGraphData";

# Get file content
$query = new CGI; 
$upload_filehandle = $query->upload("file");

# call from 
$callfrom  = $query->param("callfrom");
# directory name
$procname  = $query->param("procname");
# here the web_sm_calc-pl is called as a standalone
if( $callfrom eq "standalone"){
       $procname='';	
       $directory="$dirMAD/$uname";
       # check if the Cards directory exists
       if(! (-d  "$dirMAD/$uname/Cards")){system(" mkdir $dirMAD/$uname/Cards")}
       if(! (-d  "$dirMAD/$uname/Cards/THCFiles")){system(" mkdir $dirMAD/$uname/Cards/THCFiles")}
       $temp_id=0;
       $paramcard="param_card_$temp_id.dat";
       while (-e "$dirMAD/$uname/Cards/$paramcard"){
       $temp_id++;
       $paramcard="param_card_$temp_id.dat";
       }
       
}
# here the web_sm_calc-pl is called in setting up the web generation
 elsif( $callfrom eq "webgeneration"){
 if(! (-d  "$dirMAD/$uname/Cards")){system(" mkdir $dirMAD/$uname/Cards")}
 if(! (-d  "$dirMAD/$uname/Cards/THCFiles")){system(" mkdir $dirMAD/$uname/Cards/THCFiles")}
 $directory = "$dirMAD/$uname/$procname";
 $paramcard="param_card.dat";
 }

if ( $query->param("basis") eq "generic") {
# Start the HTML file
print "Content-type: text/html\n\n"; 
print "<HTML>\n";
print "<BODY BGCOLOR=#FFFFFF>\n";

#WARNING: We should do a check on upload_filehandle before copying it!

# Prepare a file to copy data
$base_adr = "$dirMAD/$uname/Cards/THCFiles/";
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)=localtime(time);
$time_tag=sprintf("%4d%02d%02d%02d%02d%02d",$year+1900,$mon+1,$mday,$hour,$min,$sec);
$adr=$base_adr.'sbinput'.$time_tag.'.txt';
open(myoutfile,">$adr") || print "Can't open parameter file for writing";

# Copy & close
foreach $line (<$upload_filehandle>)
 {
   print myoutfile $line;
 }
close(myoutfile);
print "Your data has been successfully uploaded and can be now downloaded <a href=/MadGraphData/$uname/Cards/THCFiles/sbinput$time_tag.txt>here</a><br>";

# Prepare output files
$adr_outputg2hb=$base_adr.'input'.$time_tag.'.txt';
$adr_logg2hb=$base_adr.'gen2hb'.$time_tag.'.log';
# Build system call string for Gen2HB
@args_gen2hb=("$ENV{'MADGRAPH_BASE'}/MG_ME/WebBin/Calculators/gen2hb", $adr, $adr_outputg2hb,$adr_logg2hb,'1e-'.$fields{precision});
# Run Gen2HB
system(@args_gen2hb) == 0 or print "system @args failed: $?";

print "Your data has been sent to gen2hb, ";

# Verify if everything is ok with the log file
open(myinfile,"<$adr_logg2hb") || print "Can't open $adr_logg2hb for reading";
@result=<myinfile>;
$err_tag=0;
foreach $line (@result)
{	
    if (index($line,'ERROR') != -1) {
	print "<pre>";print $line;print "</pre>";
	$err_tag=1;
    }

}
if ( $err_tag != 0 ) 
{
	print "but it did not manage to process your file due to the above errors!<br>";
	if( $callfrom eq "webgeneration"){
    print "<h2><center><a  href=\"http://$server/$dirHTML/$uname/$procname/HTML/web_run_form.html\" >To Go Back to the Web Run Form, Click Here !</a> </center></h2><br>\n";
}
	die;
	
}
close(myinfile);

# If OK, display download link
print "and has been successfully processed, you can download the result <a href=/MadGraphData/$uname/Cards/THCFiles/input$time_tag.txt>here</a><br>";

# Output=new input
$adr=$adr_outputg2hb;
# Prepare new output files
$adr2=$base_adr.'output'.$time_tag.'.txt';
$logadr=$base_adr.'twohiggscalc'.$time_tag.'.log';
# Build system call string
@args = ("$ENV{'MADGRAPH_BASE'}/MG_ME/WebBin/Calculators/TwoHiggsCalc", $adr, $adr2,$logadr);
# Run TwoHiggsCalc
system(@args) == 0 or print "system @args failed: $?";

print "Your data has been sent to TwoHiggsCalc, ";

# Check if everything is OK in log file
open(myinfile,"<$logadr") || print "Can't open $logadr for reading";

@result=<myinfile>;
foreach $line (@result)
{
    if (index($line,'ERROR') != -1) {
	print "but it did not manage to process your file :<br>";
	print $line;
	if( $callfrom eq "webgeneration"){
    print "<h2><center><a  href=\"http://$server/$dirHTML/$uname/$procname/HTML/web_run_form.html\" >To Go Back to the Web Run Form, Click Here !</a> </center></h2><br>\n";
}
	die;
    }

}
close(myinfile);

system("cp $adr2 $directory/Cards/$paramcard");

# If OK display download link
print "and has been successfully processed, you can download the result <a href=/MadGraphData/$uname/$procname/Cards/$paramcard>here</a><br>";

if( $callfrom eq "webgeneration"){
    print "This file is now the param_card of your process.<br>";
    print "<h2><center><a  href=\"http://$server/$dirHTML/$uname/$procname/HTML/web_run_form.html\" >To Go Back to the Web Run Form, Click Here !</a> </center></h2><br>\n";
} else {

# Write the result
print "TwoHiggsCalc results :<br>";
print "<br><pre>";

open(myinfile,"<$adr2") || print "Can't open $adr2 for reading";

@result=<myinfile>;

foreach $line (@result)
{
    print $line;
} 
close(myinfile);
}
print "</pre>";
print "</BODY></HTML>";
} else {
# HTML header
print "Content-type: text/html\n\n"; 
print "<HTML>\n";
print "<BODY BGCOLOR=#FFFFFF>\n";

#We should do a check on upload_filehandle before copying it!

# Prepare the input file
$base_adr = "$dirMAD/$uname/Cards/THCFiles/";
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)=localtime(time);
$time_tag=sprintf("%4d%02d%02d%02d%02d%02d",$year+1900,$mon+1,$mday,$hour,$min,$sec);
$adr=$base_adr.'input'.$time_tag.'.txt';
open(myoutfile,">$adr") || print "Can't open parameter file for writing";
foreach $line (<$upload_filehandle>)
 {
   	print myoutfile $line;
 } 
close(myoutfile);
print "Your data has been successfully uploaded and can be now downloaded <a href=/MadGraphData/$uname/Cards/THCFiles/input$time_tag.txt>here</a><br>";

# Prepare output files
$adr2=$base_adr.'output'.$time_tag.'.txt';
$logadr=$base_adr.'twohiggscalc'.$time_tag.'.log';

# Call TwoHiggsCalc
@args = ("$ENV{'MADGRAPH_BASE'}/MG_ME/WebBin/Calculators/TwoHiggsCalc", $adr, $adr2,$logadr);
system(@args) == 0 or print "system @args failed: $?";
print "Your data has been sent to TwoHiggsCalc, ";

# Check for errors in log file
open(myinfile,"<$logadr") || print "Can't open $logadr for reading";
@result=<myinfile>;
foreach $line (@result)
{
    if (index($line,'ERROR') != -1) {
	print "but it did not manage to process your file :<br>";
	print $line;
	if( $callfrom eq "webgeneration"){
    print "<h2><center><a  href=\"http://$server/$dirHTML/$uname/$procname/HTML/web_run_form.html\" >To Go Back to the Web Run Form, Click Here !</a> </center></h2><br>\n";
}
	die;
    }

} 
close(myinfile);

system("cp $adr2 $directory/Cards/$paramcard");

print "and has been successfully processed, you can download the result <a href=/MadGraphData/$uname/$procname/Cards/$paramcard>here</a><br>";

if( $callfrom eq "webgeneration"){
    print "This file is now the param_card of your process.<br>";
    print "<h2><center><a  href=\"http://$server/$dirHTML/$uname/$procname/HTML/web_run_form.html\" >To Go Back to the Web Run Form, Click Here !</a> </center></h2><br>\n";
} else {

# Display the output file
print "TwoHiggsCalc results :<br>";
print "<br><pre>";
open(myinfile,"<$adr2") || print "Can't open $adr2 for reading";
@result=<myinfile>;
foreach $line (@result)
{
    print $line;
} 
close(myinfile);
}
print "</pre>";
print "</BODY></HTML>";
}
