#!/usr/bin/perl -w

use Date::Manip;

$server  = $ENV{'SERVER_NAME'};
$dirMAD  = $ENV{'MADGRAPH_DATA'};
$dirBASE  = $ENV{'MADGRAPH_BASE'};

print "Content-type: text/html\n\n"; 
print   "<HTML> <HEAD> \n";
print  "<META HTTP-EQUIV=\"Pragma\" CONTENT=\"no-cache\" > ";
print  "\n <TITLE>Administration page for the Data Base</TITLE></HEAD> \n <BODY>";


    # list valid directories (when the dir name is in the user pwd file)
    my(@dirs);
    @dirs = <$dirMAD/*>;

    open(USERS,"<$dirBASE/Users/users") || print "Can't open $dirBASE/Users/users for reading";;
    @users=<USERS>;
    close(USERS);

    $i=0;
    foreach $d (@dirs) {
    @split_dir=split('/',$d);
	    BREAK: foreach $user (@users) {
		@split_users=split(':',$user);
		if($split_dir[$#split_dir] eq $split_users[0]){
			$temp[$i]=$d;
			$i++;
			last BREAK;
		}
	    }
    }
    @dirs=@temp;		
    $ndirs=$#dirs+1;

  # Write header
  print  "<H2 align=center> Process database for all users</H2>\n";
  print  "<center> $ndirs user directories.</center>";
  print  "<TABLE BORDER=2 ALIGN=CENTER>";
  print  "<TR ALIGN=CENTER>";
  print  "<TH nowrap   font color=\"\#0000FF\"> User \n";
  print  "<TH nowrap   font color=\"\#0000FF\"> # Saved Cards \n";
  print  "<TH nowrap   font color=\"\#0000FF\"> # Processes \n";
  print  "<TH nowrap   font color=\"\#0000FF\"> # Runs \n";
  print  "<TH nowrap   font color=\"\#0000FF\"> # Generating \n";
  print  "<TH nowrap   font color=\"\#0000FF\"> # Running \n";
  print  "<TH nowrap   font color=\"\#0000FF\"> Last process generation date\n";

#
# start looping over the user directories and grabbing informations
#
     foreach $dir (@dirs){

	# Number of processes
	my(@procdirs);
    	@procdirs = <$dir/PROC*>;
    	$nprocdirs=$#procdirs+1;

       # Number of cards
    	@cards = <$dir/Cards/param_card*>;
    	$ncards=$#cards+1;

	# Username
	@split_dir=split('/',$dir);
	$uname=$split_dir[$#split_dir];

	# Loop over process to catch # runs, # running and latest generation date
	$nruns=0;
	$status=0;
	$date='Tue Apr 01 00:00:00 CEST 1900';
        $ngen=0;
	foreach $procdir (@procdirs) {
		chdir($procdir);
		@runs=();
   		chdir("Events/");
		foreach $f (<*unweighted_events.lhe.gz>) { 
     	   		if ($f =~ /(.+)unweighted_events.lhe.gz/) {push(@runs,$1)}
        	}
		foreach $f (<*unweighted_events.dat.gz>) { 
     	   		if ($f =~ /(.+)unweighted_events.dat.gz/) {push(@runs,$1)}
        	}    
        	chdir("../");
        	$nruns=$nruns+$#runs+1;

		if (-e "RunWeb") {$status++;}
		if (-e "generating") {$ngen++;}

 		if (open FILE, "CREATED"){
 		@created = <FILE>;
 		$date_temp=$created[0];
 		chomp($date_temp);
 		#if(Date_Cmp($date_temp,$date)>0){$date=$date_temp;}
                $date=$date_temp;
 		}
 		close FILE;
		
	}

      # Display the result for the current dir
      print  "<TR>";
      print  "<TD align=center><a href=\"http:\/\/$server\/cgi-bin\/Admin\/print_user_db.cgi?uname=$uname\">$uname</a></TD>\n";
      print  "<TD align=center>$ncards</TD>\n";
      print  "<TD align=center>$nprocdirs</TD>\n";
      print  "<TD align=center>$nruns</TD>\n";
      print  "<TD align=center>$ngen</TD>\n";
      print  "<TD align=center>$status</TD>\n";
      print  "<TD align=center>$date</TD>\n";
      print  "</TD></TR>\n";
      print  "</TR>\n";

     }
  print  "</TABLE>\n";
  print  "\<BR\> \n";



  print  "\</BODY> \n";
  print  "\</HTML> \n";
