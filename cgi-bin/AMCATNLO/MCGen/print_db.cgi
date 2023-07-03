#!/usr/bin/perl -w

use File::stat; 
use Time::localtime; 

$server  = $ENV{'SERVER_NAME'};
$dirMC  = "/nfs/madgraph/AMCATNLO/";#$ENV{'MADGRAPH_DATA'};


#blog the directories
# my @dirs;
# foreach my $dir (<$dirMAD/$ENV{'REMOTE_USER'}/PROC*>) {
#   next unless -d $dir;
#   push @dirs,$dir;
# }

@uns_dirs = <$dirMC/$ENV{'REMOTE_USER'}/*-db-*>;
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
  print "<font  color=\"#AA0000\" face=\"Courier\"><br><br> ";
  print "	Process DataBase of $ENV{'REMOTE_USER'}:<br><br><hr></font> ";
  print "      <font  color=\"#000000\" face=\"Courier\"><p style=\"line-height:150%;\"><p>";
#  if (-d "$dirMAD/$ENV{'REMOTE_USER'}/Cards") {
#    @cardsdirs = <$dirMAD/$ENV{'REMOTE_USER'}/Cards/param_card*>;
#    $ncardsdirs=$#cardsdirs+1;
#  print  "<center>You have $ncardsdirs saved parameters cards, go to your <a href=\"http:\/\/$server\/MadGraphData\/$ENV{'REMOTE_USER'}\/Cards\">Cards</a> directory to see them.<BR>";
#  print  "To delete the Cards directory and all its content: <FORM METHOD=\"POST\" \n";
#  print  "ACTION=\"http://$server/cgi-bin/NewProcess/deldir-pl\" > \n";
#  print  "<INPUT TYPE=HIDDEN NAME=dir_name  VALUE=\"$dirMAD/$ENV{'REMOTE_USER'}/Cards\"> \n";
#  print  "<INPUT TYPE=HIDDEN NAME=url_name  VALUE=http://$server/cgi-bin/NewProcess/print_db.cgi\"> \n";
#  print  "<INPUT TYPE=SUBMIT VALUE=\"Delete Cards\"></form></center>\n";}
#
#  if (-d "$dirMAD/$ENV{'REMOTE_USER'}/Plotting") {
#    @plotdirs = <$dirMAD/$ENV{'REMOTE_USER'}/Plotting/Plot_*>;
#    $nplotdirs=$#plotdirs+1;
#  print  "<BR><center>You have $nplotdirs saved plot pages, go to your <a href=\"http:\/\/$server\/MadGraphData\/$ENV{'REMOTE_USER'}\/Plotting\">Plotting</a> directory to see them.<BR>";
#  print  "To delete the Plotting directory and all its content: <FORM METHOD=\"POST\" \n";
#  print  "ACTION=\"http://$server/cgi-bin/NewProcess/deldir-pl\" > \n";
#  print  "<INPUT TYPE=HIDDEN NAME=dir_name  VALUE=\"$dirMAD/$ENV{'REMOTE_USER'}/Plotting\"> \n";
#  print  "<INPUT TYPE=HIDDEN NAME=url_name  VALUE=http://$server/cgi-bin/NewProcess/print_db.cgi\"> \n";
#  print  "<INPUT TYPE=SUBMIT VALUE=\"Delete Plots\"></form></center>\n";}

#  if (-d "$dirMAD/$ENV{'REMOTE_USER'}/Decay") {
#    @decayfiles = <$dirMAD/$ENV{'REMOTE_USER'}/Decay/*.lhe.gz>;
#    $ndecayfiles=$#decayfiles+1;
#  print  "<BR><center>You have $ndecayfiles saved decayed event files, go to your <a href=\"http:\/\/$server\/MadGraphData\/$ENV{'REMOTE_USER'}\/Decay\">Decay</a> directory to see them.<BR>";
#  print  "To delete the Decay directory and all its content: <FORM METHOD=\"POST\" \n";
#  print  "ACTION=\"http://$server/cgi-bin/NewProcess/deldir-pl\" > \n";
#  print  "<INPUT TYPE=HIDDEN NAME=dir_name  VALUE=\"$dirMAD/$ENV{'REMOTE_USER'}/Decay\"> \n";
#  print  "<INPUT TYPE=HIDDEN NAME=url_name  VALUE=http://$server/cgi-bin/NewProcess/print_db.cgi\"> \n";
#  print  "<INPUT TYPE=SUBMIT VALUE=\"Delete decayed LHEF\"></form></center>\n";}

  print  "<center> $ndirs processes available.</center>";
  print  "<TABLE BORDER=2 ALIGN=CENTER cellpadding=5>";
  print  "<TR ALIGN=CENTER>";
  print  "<TH nowrap   font color=\"\#0000FF\"> Process: \n";
  print  "<TH nowrap   font color=\"\#0000FF\"> Collider: \n";
  print  "<TH nowrap   font color=\"\#0000FF\"> Events: \n";
  print  "<TH nowrap   font color=\"\#0000FF\"> Submitted on: \n";
#  print  "<TH nowrap   font color=\"\#0000FF\"> Keywords \n";
  print  "<TH nowrap   font color=\"\#0000FF\"> Status:\n";
  print  "<TH nowrap   font color=\"\#0000FF\"> Delete:\n";
#
# start looping over the directories and grabbing informations
#
     foreach $dir (@dirs){

	@split_dir=split('/',$dir);
	$procid=$split_dir[$#split_dir];
    
	# Go to the right PROC directory
	chdir($dir);
	# Read the proc_card
        if (open FILE, "index.html"){
             @incard=<FILE>;
             close (FILE); 
        }else{
	    @incard = ("Error: input.dat not present");
	}

	$curr_line=0;
        $orders="";
	while (index($incard[$curr_line],'aMC@NLO online MC generation:') == -1 && $curr_line < $#incard) {
		$curr_line+=1;
	} 
	if ($curr_line != $#incard) {
	    while ((index($incard[$curr_line],'>') == -1 || index($incard[$curr_line],'#') == 0) && $curr_line < $#incard) {
		$curr_line+=1;
	} 
		@div_line=split(':',$incard[$curr_line]);
		$process=$div_line[1];
		$curr_line++;
	} else {
		$process='Not found!';
	}

	$curr_line=0;


	if (open FILE, "process.dat") {
             @incard=<FILE>;
             close (FILE); 
             $process=$incard[0];
	} else {
		$process='??';
	}

	if (open FILE, "status.dat") {
             @incard=<FILE>;
             close (FILE); 
             $status=$incard[0];
	} else {
		$status='??';
	}

	if (open FILE, "coll_id.dat") {
             @incard=<FILE>;
             close (FILE); 
             $coll_id=$incard[0];
	} else {
		$coll_id='??';
	}

       if (substr($coll_id,0,3) eq 'TEV'){
           $coll_id = 'Tevatron, 1.96 TeV';
	} elsif (substr($coll_id,0,5) eq 'LHC14'){
           $coll_id = 'LHC, 14 TeV';
	}  elsif (substr($coll_id,0,4) eq 'LHC7'){
           $coll_id = 'LHC, 7 TeV';
	} 

	if (open FILE, "n_events.dat") {
             @incard=<FILE>;
             close (FILE); 
             $nevts=$incard[0];
	$date_string = ctime(stat('n_events.dat')->mtime); 
	} else {
		$nevts='??';
                $date_string='';
	}

	$date=$date_string;


#	@runs=();
#   	chdir("Events/");
#	foreach $f (<*unweighted_events.lhe.gz>) { 
#     	   if ($f =~ /(.+)unweighted_events.lhe.gz/) {push(@runs,$1)}
#        } 
#	foreach $f (<*unweighted_events.dat.gz>) { 
#     	   if ($f =~ /(.+)unweighted_events.dat.gz/) {push(@runs,$1)}
#        }  
#        chdir("../");
#        $nruns=$#runs+1;

      print  "<TR>";
#      print  "<TD align=center><a href=\"http:\/\/$server\/AMCATNLO\/AMCATNLOdata\/$ENV{'REMOTE_USER'}\/$procid\/index.html\">$process</a></TD>\n";
      print  "<TD align=center><a href=\"http:\/\/$server\/AMCATNLO\/AMCATNLOdata\/$ENV{'REMOTE_USER'}\/$procid\/index.html\">$process</a></TD>\n";
      print  "<TD align=center>$coll_id</TD>\n";
      if ( -e "allevents.tar.gz"){
      print  "<TD align=center><a href=\"http:\/\/$server\/AMCATNLO\/AMCATNLOdata\/$ENV{'REMOTE_USER'}\/$procid\/allevents.tar.gz\">$nevts</a></TD>\n";
} 
else {
      print  "<TD align=center>$nevts</TD>\n";
}
      print  "<TD align=center>$date</TD>\n";
 #     print  "<TD>$keywords</TD>\n";
      print  "<TD align=center>$status</TD>\n";
#      if($status ne "Completed") {
#      print  "<TD align=center>";
#      print  "<FORM ACTION=\"/cgi-bin/RunProcess/handle_jobs-pl\" METHOD=\"POST\">";
#      print  "<INPUT TYPE=HIDDEN NAME=dir_name VALUE=\"$dir\"> \n";
#      print  "<INPUT TYPE=HIDDEN NAME=whattodo VALUE=\"kill\"> \n";
#      print  "<INPUT TYPE=SUBMIT VALUE=\"Stop Job\">  \n";
#      print  "</FORM> \n";
#	}
      if(substr($status,0,9) eq "Completed" || $status eq "??") {
      print  "<TD align=center> <FORM METHOD=\"POST\" \n";
      print  "ACTION=\"http://$server/cgi-bin/NewProcess/deldir-pl\" > \n";
      print  "<INPUT TYPE=HIDDEN NAME=dir_name  VALUE=\"$dir\"> \n";
      print  "<INPUT TYPE=HIDDEN NAME=url_name  VALUE=http://$server/cgi-bin/AMCATNLO/MCGen/print_db.cgi\"> \n";
      print  "<INPUT TYPE=SUBMIT VALUE=\"Delete dir\"></form>\n";
      }

      print  "</TD></TR>\n";
      print  "</TR>\n";

     }

  print  "</TABLE>\n";
  print "<FORM METHOD=\"POST\" \n";
      print  "ACTION=\"http://$server/cgi-bin/AMCATNLO/MCGen/cleandir-pl\" > \n";
      print  "<INPUT TYPE=HIDDEN NAME=dir_name  VALUE=\"$dir\"> \n";
      print  "<INPUT TYPE=HIDDEN NAME=url_name  VALUE=http://$server/cgi-bin/AMCATNLO/MCGen/print_db.cgi\"> \n";
      print  "<INPUT TYPE=SUBMIT VALUE=\"CLEAN USER DIR\"></form>\n";  
  print  "\<BR\> \n";
  print  "\</BODY> \n";
  print  "\</HTML> \n";
