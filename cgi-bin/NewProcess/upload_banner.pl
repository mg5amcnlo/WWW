#!/usr/bin/perl -w

use CGI;

$dirDATA = $ENV{'MADGRAPH_DATA'};
$uname = $ENV{REMOTE_USER};
$bin = "$ENV{'MADGRAPH_BASE'}/MG_ME/WebBin";
$server = $ENV{'SERVER_NAME'};

if(! -e "$dirDATA/$uname" ){ 
system("mkdir $dirDATA/$uname")};
$directory="$dirDATA/$uname";

$query = new CGI; 

$upload_filehandle = $query->upload("banner");
@content=<$upload_filehandle>;
open(FILE,">$directory/banner.txt") or WriteError("File $directory/$file_name could not be opened");

foreach $line (@content) {
    
    if(length($line)>1){print FILE $line};
    
}
close FILE;

#open UPLOADFILE, ">$directory/banner.txt";
#while ( <$upload_filehandle> )
#{
#   print UPLOADFILE;
#}
#close UPLOADFILE;

chdir("$directory");

system("tr -d '\r' < banner.txt > tmp.txt; mv -f tmp.txt banner.txt");
system("$bin/split_banner.pl banner.txt > /dev/null");

open(CARD,"<$directory/proc_card_mg5.dat") or
  WriteError("The resulting proc_card could not be opened.");

print $query->header ( ); 

print "<HTML> <HEAD> \n";
print "</HEAD>\n";
print "<FORM METHOD=\"POST\" ACTION=\"http://$server/cgi-bin/NewProcess/webnewprocess-pl\" >\n";
print "<INPUT TYPE=HIDDEN NAME=callfrom  VALUE=\"upload_banner\">\n";
print "<CENTER> File upload ok <BR>";
print "<INPUT TYPE=SUBMIT VALUE=\"Continue\"> \n";
print "</center>";
print "<BR><hr>\n";
print "Resulting proc_card.dat:\n";
print "<PRE>\n";
while(<CARD>){
  print "$_";
}
print "</PRE>\n";
close CARD;
print "</BODY></HTML> \n \n";

sub WriteError {
$errormsg=shift;
$| = 1;
print "Content-type: text/html\n\n";
print "<HTML>\n<HEAD> \n";
print "<TITLE>Proc card upload failed</TITLE></HEAD>\n<BODY>\n";
print "<H2>Proc upload failed</H2>\n";
print "$errormsg";
print "<BR>Click the \"backwards\" button in you browser and correct the error.\n";
print "</BODY>\n</HTML>\n";
die;
}
