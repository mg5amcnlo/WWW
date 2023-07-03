#!/usr/bin/perl -w

$server  = $ENV{'SERVER_NAME'};
$dirMAD  = $ENV{'MADGRAPH_DATA'};


#blog the directories
# my @dirs;
# foreach my $dir (<$dirMAD/$ENV{'REMOTE_USER'}/PROC*>) {
#   next unless -d $dir;
#   push @dirs,$dir;
# }

@uns_dirs = <$dirMAD/$ENV{'REMOTE_USER'}/PROC*>;
my @dirs = sort {
      -M $a <=> -M $b
}@uns_dirs;
$ndirs=$#dirs+1;

#
# name of the output file
#
  print "Content-type: text/html\n\n"; 
  print   "<HTML> <HEAD> \n";
  print  "<META HTTP-EQUIV=\"Pragma\" CONTENT=\"no-cache\" > ";
  print  "\n <TITLE>Data Base</TITLE></HEAD> \n <BODY>";
  print  "<H2 align=center> Database of $ENV{'REMOTE_USER'}</H2>\n";
  if (-d "$dirMAD/$ENV{'REMOTE_USER'}/Cards") {
    @cardsdirs = <$dirMAD/$ENV{'REMOTE_USER'}/Cards/param_card*>;
    $ncardsdirs=$#cardsdirs+1;
  print  "<BR><center>You have $ncardsdirs saved parameters cards, go to your <a href=\"http:\/\/$server\/MadGraphData\/$ENV{'REMOTE_USER'}\/Cards\">Cards</a> directory to see them.<BR>";
  print  "To delete the Cards directory and all its content: <FORM METHOD=\"POST\" \n";
  print  "ACTION=\"http://$server/cgi-bin/NewProcess/deldir-pl\" > \n";
  print  "<INPUT TYPE=HIDDEN NAME=dir_name  VALUE=\"$dirMAD/$ENV{'REMOTE_USER'}/Cards\"> \n";
  print  "<INPUT TYPE=HIDDEN NAME=url_name  VALUE=http://$server/cgi-bin/NewProcess/print_db.cgi\"> \n";
  print  "<INPUT TYPE=SUBMIT VALUE=\"Delete Cards\"></form></center>\n";}

  if (-d "$dirMAD/$ENV{'REMOTE_USER'}/Plotting") {
    @plotdirs = <$dirMAD/$ENV{'REMOTE_USER'}/Plotting/Plot_*>;
    $nplotdirs=$#plotdirs+1;
  print  "<BR><center>You have $nplotdirs saved plot pages, go to your <a href=\"http:\/\/$server\/MadGraphData\/$ENV{'REMOTE_USER'}\/Plotting\">Plotting</a> directory to see them.<BR>";
  print  "To delete the Plotting directory and all its content: <FORM METHOD=\"POST\" \n";
  print  "ACTION=\"http://$server/cgi-bin/NewProcess/deldir-pl\" > \n";
  print  "<INPUT TYPE=HIDDEN NAME=dir_name  VALUE=\"$dirMAD/$ENV{'REMOTE_USER'}/Plotting\"> \n";
  print  "<INPUT TYPE=HIDDEN NAME=url_name  VALUE=http://$server/cgi-bin/NewProcess/print_db.cgi\"> \n";
  print  "<INPUT TYPE=SUBMIT VALUE=\"Delete Plots\"></form></center>\n";}

  if (-d "$dirMAD/$ENV{'REMOTE_USER'}/Decay") {
    @decayfiles = <$dirMAD/$ENV{'REMOTE_USER'}/Decay/*.lhe.gz>;
    $ndecayfiles=$#decayfiles+1;
  print  "<BR><center>You have $ndecayfiles saved decayed event files, go to your <a href=\"http:\/\/$server\/MadGraphData\/$ENV{'REMOTE_USER'}\/Decay\">Decay</a> directory to see them.<BR>";
  print  "To delete the Decay directory and all its content: <FORM METHOD=\"POST\" \n";
  print  "ACTION=\"http://$server/cgi-bin/NewProcess/deldir-pl\" > \n";
  print  "<INPUT TYPE=HIDDEN NAME=dir_name  VALUE=\"$dirMAD/$ENV{'REMOTE_USER'}/Decay\"> \n";
  print  "<INPUT TYPE=HIDDEN NAME=url_name  VALUE=http://$server/cgi-bin/NewProcess/print_db.cgi\"> \n";
  print  "<INPUT TYPE=SUBMIT VALUE=\"Delete decayed LHEF\"></form></center>\n";}

  print  "<center> $ndirs processes available.</center>";
  print  "<TABLE BORDER=2 ALIGN=CENTER>";
  print  "<TR ALIGN=CENTER>";
  print  "<TH nowrap   font color=\"\#0000FF\"> Process \n";
  print  "<TH nowrap   font color=\"\#0000FF\"> Model \n";
  print  "<TH nowrap   font color=\"\#0000FF\"> Order \n";
  print  "<TH nowrap   font color=\"\#0000FF\"> Date \n";
#  print  "<TH nowrap   font color=\"\#0000FF\"> Keywords \n";
  print  "<TH nowrap   font color=\"\#0000FF\"> Available runs\n";
  print  "<TH nowrap   font color=\"\#0000FF\"> Status\n";
  print  "<TH nowrap   font color=\"\#0000FF\"> Delete\n";
#
# start looping over the directories and grabbing informations
#
     foreach $dir (@dirs){

	@split_dir=split('/',$dir);
	$procid=$split_dir[$#split_dir];
    
	# Go to the right PROC directory
	chdir($dir);
	# Read the proc_card
        if (open FILE, "SubProcesses/procdef_mg5.dat"){
             @incard=<FILE>;
             close (FILE); 
             $model = "MG5/ ";               
        } elsif (open FILE, "Cards/proc_card.dat"){
             @incard=<FILE>;
             close (FILE);
             $model = "";
        } else{
	    @incard = ("Error: input.dat not present");
	}

	$curr_line=0;
        $orders="";
	while (index($incard[$curr_line],'Begin PROCESS') == -1 && $curr_line < $#incard) {
		$curr_line+=1;
	} 
	if ($curr_line != $#incard) {
	    while ((index($incard[$curr_line],'>') == -1 || index($incard[$curr_line],'#') == 0) && $curr_line < $#incard) {
		$curr_line+=1;
	} 
		@div_line=split('#',$incard[$curr_line]);
		$process=$div_line[0];
		$curr_line++;
                while (index($incard[$curr_line],'end_coup') == -1 && $curr_line < $#incard) {
		@div_line=split('#',$incard[$curr_line]);
		$orders=$orders." ".$div_line[0];
                $curr_line++;
                }
	} else {
		$process='Not found!';
		$orders='??';
	}

	$curr_line=0;
	while (index($incard[$curr_line],'Begin MODEL') == -1 && $curr_line < $#incard) {
		$curr_line+=1;
	} 
	if ($curr_line != $#incard) {
		$curr_line++;
		@div_line=split('#',$incard[$curr_line]);
		$model.=$div_line[0];
	} else {
		$model.='??';
	}
	
        if (open FILE, "index.html"){
            @card = <FILE>;
        }
        else{
            @card = ("Error: index.html not present");
        }
        close FILE;

	foreach $i (@card){
	    if($i =~ /Keywords:(.+)/){$keywords=$1;chomp($keywords); }
	}
	
 	foreach $i (@card){
 	    if($i =~ /Created: <\/b>(.+)/){$created=$1;chomp($created); }
 	}

 	@full_date=split(/<\/font>/,$created);
	$date=$full_date[0];

	if (-e "RunWeb") {$status ="running"}
 	else {$status = "not running"};

	@runs=();
   	chdir("Events/");
	foreach $f (<*unweighted_events.lhe.gz>) { 
     	   if ($f =~ /(.+)unweighted_events.lhe.gz/) {push(@runs,$1)}
        } 
	foreach $f (<*unweighted_events.dat.gz>) { 
     	   if ($f =~ /(.+)unweighted_events.dat.gz/) {push(@runs,$1)}
        }  
        chdir("../");
        $nruns=$#runs+1;

      print  "<TR>";
      print  "<TD align=center><a href=\"http:\/\/$server\/MadGraphData\/$ENV{'REMOTE_USER'}\/$procid\/\">$process</a></TD>\n";
      print  "<TD align=center>$model</TD>\n";
      print  "<TD align=center>$orders</TD>\n";
      print  "<TD align=center>$date</TD>\n";
 #     print  "<TD>$keywords</TD>\n";
      print  "<TD align=center><a href=\"http:\/\/$server\/MadGraphData\/$ENV{'REMOTE_USER'}\/$procid\/crossx.html\">$nruns</a></TD>\n";
      print  "<TD align=center>$status</TD>\n";
      if($status eq "running") {
      print  "<TD align=center>";
      print  "<FORM ACTION=\"/cgi-bin/RunProcess/handle_jobs-pl\" METHOD=\"POST\">";
      print  "<INPUT TYPE=HIDDEN NAME=dir_name VALUE=\"$dir\"> \n";
      print  "<INPUT TYPE=HIDDEN NAME=whattodo VALUE=\"kill\"> \n";
      print  "<INPUT TYPE=SUBMIT VALUE=\"Stop Job\">  \n";
      print  "</FORM> \n";
	}
      if($status eq "not running") {
      print  "<TD align=center> <FORM METHOD=\"POST\" \n";
      print  "ACTION=\"http://$server/cgi-bin/NewProcess/deldir-pl\" > \n";
      print  "<INPUT TYPE=HIDDEN NAME=dir_name  VALUE=\"$dir\"> \n";
      print  "<INPUT TYPE=HIDDEN NAME=url_name  VALUE=http://$server/cgi-bin/NewProcess/print_db.cgi\"> \n";
      print  "<INPUT TYPE=SUBMIT VALUE=\"Delete dir\"></form>\n";
      }

      print  "</TD></TR>\n";
      print  "</TR>\n";

     }

  print  "</TABLE>\n";
  print  "\<BR\> \n";
  print  "\</BODY> \n";
  print  "\</HTML> \n";
