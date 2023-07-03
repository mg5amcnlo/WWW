#!/usr/bin/perl -w

#use the CGI.pm module
#use CGI qw/:standard/;

#------------------------------------------------------------
#
#          This is Denny's code for unwrapping the form replies
#
my($zzi,@marked,$qn,$an);
binmode STDIN;
read STDIN,$zzi,$ENV{CONTENT_LENGTH};
$zzi =~ s/\s$//g;
@marked = split(/&/,$zzi);
foreach $input (@marked) {
    ($qn,$an) = split(/=/,$input);
    $an =~ tr/+/ /;
    $an =~ s/%([a-fA-F0-9][a-fA-F0-9])/pack("C", hex($1))/eg;
    $an =~ s/~!/ ~!/g;
    $in{$qn} = $an
    }


$server = $ENV{'SERVER_NAME'};

#------------------------------------------------------------
# read in the values passed from the form
#------------------------------------------------------------

# dir
$dir     =$in{dir_name};
chomp($dir);
$dir =~ s/\s//g;

$| = 1;
print "Content-type: text/html\n\n";
print  "<HTML> <HEAD> \n";
print "<META HTTP-EQUIV=\"Refresh\" CONTENT=\"3\; ";
print "URL=http:\/\/$server\/new_gen_proc_card.html\"> \n";
print  "</HEAD> <BODY> <CENTER>\n";

if (open IDDATA,"$dir/generating"){
	foreach $line (<IDDATA>){
	    print "Deleting job $line <br> \n";
	    system("qdel $line >& /dev/null");
	}
	close IDATA;
}
system("rm -f $dir/running_jobs");

print "Removing user directory $dir <br> \n";
system("rm -rf $dir");

print "<br> You are going to be redirected to the new process page...";
print "</CENTER></BODY></HTML> \n \n";
#
#   That's all folks!
#
close(STDIN);
