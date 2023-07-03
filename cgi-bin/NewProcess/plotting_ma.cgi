#!/usr/bin/perl

# Use CGI stuff
use CGI;

# Define geneic env variables
$uname  = $ENV{REMOTE_USER};
$dirBASE = $ENV{'MADGRAPH_BASE'};
$dirMAD = $ENV{'MADGRAPH_DATA'};
$server = $ENV{'SERVER_NAME'};
$dirHTML= "MadGraphData";
$MAdir="$dirBASE/MG_ME/MadAnalysis";
$TDdir="$dirBASE/MG_ME/td";

# Define default variables for upload
my ($bytesread, $buffer);
my $numbytes = 1024;

# Get files content
$query = new CGI; 


# If run for the first time (e.g. from plotting_ma.html)
if (! $query->param("dirnum")) {


	# Create plotting dir if needed
	if(! (-d  "$dirMAD/$uname/Plotting")){
	 
    	system(" mkdir $dirMAD/$uname/Plotting");
    
 	}

	# Create a new plotting subdir where we can
	$temp_id=1;
	while (-d "$dirMAD/$uname/Plotting/Plot_$temp_id"){
		$temp_id++;
	}
	$dirPlot="$dirMAD/$uname/Plotting/Plot_$temp_id";
	system("mkdir $dirPlot");
	
	
	# Tag original file name
	$file_name=$query->param("lhe_file");

	#copy content of uploaded event file to unweighted_events.lhe.gz
	$lhe_file = $query->upload("lhe_file");
	open (OUTFILE, ">", "$dirPlot/$file_name")
   		or die "Couldn't open $dirPlot/$file_name for writing: $!";
	while ($bytesread = read($lhe_file, $buffer, $numbytes)) {print OUTFILE $buffer;}
	close OUTFILE;

	# Copy MA and compile it
	chdir($dirPlot);
	system("cp $MAdir/* .;make > make.log");

# If run from index.html
} else {

	# Get dir number and original file name from index.html
	$temp_id=$query->param("dirnum");
	$dirPlot="$dirMAD/$uname/Plotting/Plot_$temp_id";
	$file_name=$query->param("filename");
	
}



# Try to upload plotting card as ma_card.dat 
$plot_card_name=$query->param("plot_card_file");
if($plot_card_name ne "") {
	$plot_card_file = $query->upload("plot_card_file");
	open (OUTFILE, ">", "$dirPlot/ma_card.dat")
   		or die "Couldn't open $dirPlot/ma_card.dat for writing: $!";
	while ($bytesread = read($plot_card_file, $buffer, $numbytes)) {print OUTFILE $buffer;}
	close OUTFILE;}
# otherwise take the default one	
else {
	system("cp $dirBASE/WWW/htdocs/EXAMPLES/Cards/ma_card.dat $dirPlot/ma_card.dat");
}

# Try to unzip events
chdir($dirPlot);
system("gunzip $dirPlot/$file_name")==0
   or die "Couldn't unzip $dirPlot/$file_name: $!";

$file_name=~s/\.[^.]*$//;

# Run MA and record output
system("echo \"$file_name\" > file_name.dat");
system("./plot_events < file_name.dat > out.log");
system("$TDdir/td plots.top >> out.log");
system("ps2pdf plots.ps >> out.log");

#try to zip events
system("gzip $dirPlot/$file_name")==0
   or die "Couldn't zip $dirPlot/$file_name: $!";
   
# Create index.html in the plotting dir
open (PAGE, ">", "$dirPlot/index.html")
   or die "Couldn't open $dirPlot/index.html for writing: $!";
print PAGE "<HTML>\n";
print PAGE "<head><title>MadAnalysis plot page for $file_name</title></head>\n";
print PAGE "<BODY BGCOLOR=#FFFFFF>\n";
print PAGE "<center><h1>Plots for your event file $file_name</h1>\n";
print PAGE "<h3>Your ma_card was uploaded as <a href=\"http://$server/$dirHTML/$uname/Plotting/Plot_$temp_id/ma_card.dat\">ma_card.dat</a></h3>";
print PAGE "<h3>You can refer to this page later as http://$server/$dirHTML/$uname/Plotting/Plot_$temp_id/index.html</h3>";
print PAGE "<h3>You can download the two MadAnalysis output log files <a href=\"http://$server/$dirHTML/$uname/Plotting/Plot_$temp_id/out.log\">here</a></h3> and <a href=\"http://$server/$dirHTML/$uname/Plotting/Plot_$temp_id/plots.log\">here</a></h3>";
print PAGE "<h3>You can download the TopDrawer file <a href=\"http://$server/$dirHTML/$uname/Plotting/Plot_$temp_id/plots.top\">here</a></h3>";
print PAGE "<h3>You can download the summary file as a <a href=\"http://$server/$dirHTML/$uname/Plotting/Plot_$temp_id/plots.ps\">ps</a> or a <a href=\"http://$server/$dirHTML/$uname/Plotting/Plot_$temp_id/plots.pdf\">pdf</a></h3>";
print PAGE "<h3>You can also upload an other ma_card to create new plots (all former plots will be lost but there is no need to upload your event file again\n";
print PAGE "<form ACTION=\"/cgi-bin/NewProcess/plotting_ma.cgi\" METHOD=\"post\"  ENCTYPE=\"multipart/form-data\"> ";
print PAGE "<INPUT TYPE=\"hidden\" NAME=\"filename\" VALUE=\"$file_name.gz\">";
print PAGE "<INPUT TYPE=\"hidden\" NAME=\"dirnum\" VALUE=\"$temp_id\"> <INPUT TYPE=\"file\" NAME=\"plot_card_file\"> <input TYPE=\"submit\" VALUE=\"Upload & Plot\"></form></h3>";

print PAGE "</center></BODY>\n";
print PAGE "</HTML>\n";
close(PAGE);

# Flush output
print "Content-type: text/html\n\n";
print "<HTML>\n";
print "<meta http-equiv=\"refresh\" content=\"2;url=http://$server/$dirHTML/$uname/Plotting/Plot_$temp_id/index.html\">";
print "<BODY BGCOLOR=#FFFFFF>\n";
print "<h3>MadAnalysis has been run, you are going to be redirected to the output...</h3>\n";
print "</center></BODY>\n";
print "</HTML>\n";
