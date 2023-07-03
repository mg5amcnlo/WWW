#!/usr/bin/perl

# Use CGI stuff
use CGI;


$uname  = $ENV{REMOTE_USER};
$dirBASE = $ENV{'MADGRAPH_BASE'};
$dirMAD = $ENV{'MADGRAPH_DATA'};
$server = $ENV{'SERVER_NAME'};
$dirHTML= "MadGraphData";
$ERAdir="$dirBASE/MG_ME/ExRootAnalysis";

# Get files content
$query = new CGI; 
$lhe_file = $query->upload("lhe_file");
$decay = $query->param('decay');

# Create decay dir if needed
if(! (-d  "$dirMAD/$uname/Decay")){system(" mkdir $dirMAD/$uname/Decay")}

$file_name=$query->param("lhe_file");

print "Content-type: text/html\n\n"; 
print "<HTML>\n";
print "<head><title>Decay page for $file_name</title></head>\n";
print "<BODY BGCOLOR=#FFFFFF>\n";


#upload event file
my ($bytesread, $buffer);
my $numbytes = 1024;
open (OUTFILE, ">", "$dirMAD/$uname/Decay/$file_name")
   or die "Couldn't open $dirMAD/$uname/Decay/$file_name for writing: $!";
while ($bytesread = read($lhe_file, $buffer, $numbytes)) {
        print OUTFILE $buffer;
}
close OUTFILE;

#try to unzip it
system("gunzip $dirMAD/$uname/Decay/$file_name")==0
   or die "Couldn't unzip $dirMAD/$uname/Decay/$file_name: $!";

#create decay parameters file
open (OUTFILE, ">", "$dirMAD/$uname/Decay/decay_card.dat")
   or die "Couldn't open $dirMAD/$uname/Decay/decay_card.dat for writing: $!";
print OUTFILE "1\n";
$in_name=substr($file_name,0,-7).".lhe";
print OUTFILE "$in_name\n";
$namewoext=substr($file_name,0,-7);
$out_name=substr($file_name,0,-7)."_out.lhe";
print OUTFILE "$out_name\n";
@decay_tag=split(/_/,$decay);
print OUTFILE $decay_tag[0]."\n";
print OUTFILE $decay_tag[1]."\n";
close OUTFILE;

chdir("$dirMAD/$uname/Decay");
system("$dirBASE/MG_ME/DECAY/decay < decay_card.dat > log.txt"); 

system("rm -f decay_card.dat; rm -f $in_name");

if($query->param("root") eq "yes") {
system("$ERAdir/ExRootLHEFConverter $dirMAD/$uname/Decay/$out_name $dirMAD/$uname/Decay/$namewoext.root > /dev/null")==0
   or die "Couldn't apply ERA LHEF converter on $dirMAD/$uname/Decay/$out_name: $!";

}

if (-e $out_name.".gz") {system("rm $out_name.gz > /dev/null");}

system("gzip $out_name");

print "Your file has been decayed. You can download the output <a href=\"http://$server/$dirHTML/$uname/Decay/$out_name.gz\">here</a><br>";
print "The log file for this decay session is <a href=\"http://$server/$dirHTML/$uname/Decay/log.txt\">here</a><br>";


if($query->param("root") eq "yes") {
print "The output has been converted to root. You can download the result <a href=\"http://$server/$dirHTML/$uname/Decay/$namewoext.root\">here</a><br>";
}

print "</BODY>\n";
print "</HTML>\n";

