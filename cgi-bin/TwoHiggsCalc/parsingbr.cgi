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
$adr4=$base_adr.'legende'.$time_tag.'.txt';
$adr5=$base_adr.'absord'.$time_tag.'.txt';
$adr6=$base_adr.'plotbr'.$time_tag.'.eps';
$adr7=$base_adr.'comment'.$time_tag.'.ps';

open(reference,">$adr") || print "Can't open reference file for writing";
open(instructions,">$adr2") || print "Can't open instructions file for writing";


$TYPE=$query->param("type");
#print "$ENV{'MADGRAPH_BASE'}/MG_ME/Calculators/TwoHiggsCalc/bin/ref.txt";
open(blabla,"$ENV{'MADGRAPH_BASE'}/MG_ME/Calculators/TwoHiggsCalc/bin/ref.txt") || print "Can't open paramcard file for writing";
@refer=<blabla>;
close(blabla);
print "ok";
foreach $line (@refer)
{
        print reference $line;
#	print $line;
}
print "ok2";
close(reference);

print instructions "# Instruction file for THC parsing script, from the Web Interface\n\n";

print instructions "FILE=$adr\n\n";

$MIN=$query->param("min");
$MAX=$query->param("max");
$STEP=$query->param("step");
$M1=$query->param("idparama");
$M2=$query->param("idparamb");
$M3=$query->param("idparamc");
$M4=$query->param("idparamd");
$TGB=$query->param("idparame");



print instructions "0:\n";
print instructions "BLOCK=minpar\n";
print instructions "TYPE=$TYPE\n";
print instructions "M1=$M1\n";
print instructions "M2=$M2\n";
print instructions "M3=$M3\n";
print instructions "M4=$M4\n";
print instructions "TGB=$TGB\n";
print instructions "MIN=$MIN\n";
print instructions "MAX=$MAX\n";
print instructions "STEP=$STEP\n";

for($i=1; $i<=10; $i++) {
	if($query->param("idparam$i") ne ''){
		$paramid=$query->param("idparam$i");
		print instructions "$i:\n";
		print instructions "BLOCK=decay\n";
		print instructions "ID$i=$paramid\n";
	}
}

close(instructions);

system("perl $ENV{'MADGRAPH_BASE'}/MG_ME/WebBin/Calculators/parserBR.pl $adr2 $adr3 $adr4 $adr5 $adr6 $adr7");

# Display the output file
print "TwoHiggsCalc been successfully processed, you can download the result <a href=/MadGraphData/$uname/Cards/THCFiles/results$time_tag.txt>here</a><br>";
print "Br plot has been successfully processed, you can download the result <a href=/MadGraphData/$uname/Cards/THCFiles/plotbr$time_tag.eps>here</a><br>";


print "Results :<br>";
print "<br><pre>";
open(myinfile,"<$adr3") || print "Can't open $adr3 for reading (3)";
@result=<myinfile>;
foreach $line (@result)
{
    print $line;
} 
close(myinfile);
print "</pre>";
print "</BODY></HTML>";
