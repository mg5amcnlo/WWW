#!/usr/bin/perl

# Use CGI stuff
use CGI;


$uname  = $ENV{REMOTE_USER};
$dirBASE = $ENV{'MADGRAPH_BASE'};
$dirMAD = $ENV{'MADGRAPH_DATA'};
$server = $ENV{'SERVER_NAME'};
$dirHTML= "MadGraphData";
$ERAdir="$dirBASE/MG_ME/ExRootAnalysis";
my ($bytesread, $buffer);
my $numbytes = 1024;

# Get files content
$query = new CGI; 

if (! $query->param("dirnum")) {

$lhe_file = $query->upload("lhe_file");
# Create plotting dir if needed
if(! (-d  "$dirMAD/$uname/Plotting")){system(" mkdir $dirMAD/$uname/Plotting")}

# Create a new plotting subdir where we can
$temp_id=0;
while (-d "$dirMAD/$uname/Plotting/Plot_$temp_id"){
$temp_id++;
}
$dirPlot="$dirMAD/$uname/Plotting/Plot_$temp_id";
system("mkdir $dirPlot");
$file_name=$query->param("lhe_file");

#upload event file
open (OUTFILE, ">", "$dirPlot/unweighted_events.lhe.gz")
   or die "Couldn't open $dirPlot/unweighted_events.lhe.gz for writing: $!";
while ($bytesread = read($lhe_file, $buffer, $numbytes)) {
        print OUTFILE $buffer;
}

close OUTFILE;
#try to unzip it
system("gunzip $dirPlot/unweighted_events.lhe.gz")==0
   or die "Couldn't unzip $dirPlot/unweighted_events.lhe.gz: $!";
#create root file
system("$ERAdir/ExRootLHEFConverter $dirPlot/unweighted_events.lhe $dirPlot/unweighted_events.root > /dev/null")==0
   or die "Couldn't apply ERA LHEF converter on $dirPlot/unweighted_events.lhe: $!";
system("echo \"$dirPlot/unweighted_events.root\" > $dirPlot/events.list");
#delete events file if needed
chdir($dirPlot);
system("gzip unweighted_events.lhe > /dev/null");

} else {
$temp_id=$query->param("dirnum");
$dirPlot="$dirMAD/$uname/Plotting/Plot_$temp_id";
$file_name=$query->param("filename");
}



$plot_card_name=$query->param("plot_card_file");

if($plot_card_name ne "") {
$plot_card_file = $query->upload("plot_card_file");

#upload plot_card file
open (OUTFILE, ">", "$dirPlot/plot_card.dat")
   or die "Couldn't open $dirPlot/plot_card.dat for writing: $!";
while ($bytesread = read($plot_card_file, $buffer, $numbytes)) {
        print OUTFILE $buffer;
}
close OUTFILE;}
else {
system("cp $dirBASE/WWW/htdocs/EXAMPLES/Cards/plot_card.dat $dirPlot/plot_card.dat");
$plot_card_name="plot_card.dat";
}

#create index.html in it
open (PAGE, ">", "$dirPlot/index.html")
   or die "Couldn't open $dirPlot/index.html for writing: $!";




print PAGE "<HTML>\n";
print PAGE "<head><title>Plot page for $file_name</title></head>\n";
print PAGE "<BODY BGCOLOR=#FFFFFF>\n";
print PAGE "<center><h1>Plots for your event file $file_name</h1>\n";
print PAGE "<h3>Your plot_card was <a href=\"http://$server/$dirHTML/$uname/Plotting/Plot_$temp_id/plot_card.dat\">$plot_card_name</a></h3>";
print PAGE "<h3>You can refer to this page later as http://$server/$dirHTML/$uname/Plotting/Plot_$temp_id/index.html</h3>";
print PAGE "<h3>You can download the output ROOT file <a href=\"http://$server/$dirHTML/$uname/Plotting/Plot_$temp_id/unweighted_events.root\"> here</a></h3>";
print PAGE "<h3>To re-plot these events with a different plot_card, upload it here";
print PAGE "<form ACTION=\"/cgi-bin/NewProcess/plotting.cgi\" METHOD=\"post\"  ENCTYPE=\"multipart/form-data\"> <INPUT TYPE=\"file\" NAME=\"plot_card_file\"> <input TYPE=\"submit\" VALUE=\"Upload & Plot\">";
print PAGE "<INPUT TYPE=\"hidden\" NAME=\"filename\" VALUE=\"$file_name\">";
print PAGE "<INPUT TYPE=\"hidden\" NAME=\"dirnum\" VALUE=\"$temp_id\"> </form></h3>";



chdir($dirPlot);
system("rm *.eps>/dev/null;rm *.jpg>/dev/null");

system("cd $dirPlot; $ERAdir/ExRootMain plot_card.dat > /dev/null")==0
   or die "Couldn't exec ExRootMain with $dirPlot/plot_card.dat: $!";

#convert plots
@eps_files = <$dirPlot/*.eps>;
foreach $eps_file (@eps_files) {
system("$ERAdir/doc/epstosmth --gsopt='-r60x60 -dGraphicsAlphaBits=4' --gsdev=jpeg $eps_file")==0
   or die "Couldn't convert eps to jpg for $eps_file: $!";
}

#create the output
foreach $f (<*.jpg>) { 
     print PAGE "\<IMG SRC=\"http://$server/$dirHTML/$uname/Plotting/Plot_$temp_id/$f\" BORDER\=1\> \<BR\> \n";
     $feps=substr($f,0,-3)."eps";
     print PAGE "\<A HREF\=\"http://$server/$dirHTML/$uname/Plotting/Plot_$temp_id/$feps\"\>EPS version\</A\> \<BR\> \<BR\> \<BR\>\n"; 
}

print PAGE "</center></BODY>\n";
print PAGE "</HTML>\n";
close(PAGE);

open(myinfile,"<$dirPlot/index.html") || print "Can't open $dirPlot/index.html for reading";
print "Content-type: text/html\n\n"; 
foreach $line (<myinfile>) {print $line;}
close(myinfile);


