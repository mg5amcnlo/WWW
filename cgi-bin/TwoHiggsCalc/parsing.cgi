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

print $query->header();
print $query->start_html;

$base_adr = "$dirMAD/$uname/Cards/THCFiles/";
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)=localtime(time);
$time_tag=sprintf("%4d%02d%02d%02d%02d%02d",$year+1900,$mon+1,$mday,$hour,$min,$sec);
$adr=$base_adr.'reference'.$time_tag.'.txt';
$adr2=$base_adr.'instructions'.$time_tag.'.txt';
$adr3=$base_adr.'results'.$time_tag.'.txt';

open(reference,">$adr") || print "Can't open reference file for writing";
open(instructions,">$adr2") || print "Can't open instructions file for writing";

foreach $line (<$upload_filehandle>)
{
   	print reference $line;
}
close(reference);

print instructions "# Instruction file for THC parsing script, from the Web Interface\n\n";

print instructions "FILE=$adr\n\n";

$block=$query->param("block");
$paramid=$query->param("idparam");
$min=$query->param("min");
$max=$query->param("max");
$step=$query->param("step");

print instructions "0:\n";
print instructions "BLOCK=$block\n";
print instructions "ID=$paramid\n";
print instructions "MIN=$min\n";
print instructions "MAX=$max\n";
print instructions "STEP=$step\n";

for($i=1; $i<=10; $i++) {
	if($query->param("block$i") ne ''){
		$block=$query->param("block$i");
		$paramid=$query->param("idparam$i");
		print instructions "$i:\n";
		print instructions "BLOCK=$block\n";
		print instructions "ID=$paramid\n";
	}
}

close(instructions);

system("perl $ENV{'MADGRAPH_BASE'}/MG_ME/WebBin/Calculators/parser.pl $adr2 $adr3");

# Display the output file
print "TwoHiggsCalc been successfully processed, you can download the result <a href=/MadGraphData/$uname/Cards/THCFiles/results$time_tag.txt>here</a><br>";

print "Results :<br>";
print "<br><pre>";
open(myinfile,"<$adr3") || print "Can't open $adr3 for reading";
@result=<myinfile>;
foreach $line (@result)
{
    print $line;
} 
close(myinfile);
print "</pre>";
print "</BODY></HTML>";
