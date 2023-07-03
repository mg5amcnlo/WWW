#!/usr/bin/perl -w

local ($buffer, @pairs, $pair, $name, $value, %FORM);
    # Read in text
    $ENV{'REQUEST_METHOD'} =~ tr/a-z/A-Z/;
    if ($ENV{'REQUEST_METHOD'} eq "GET")
    {
	$buffer = $ENV{'QUERY_STRING'};
    }
    # Split information into name/value pairs
    @pairs = split(/&/, $buffer);
    foreach $pair (@pairs)
    {
	($name, $value) = split(/=/, $pair);
	$value =~ tr/+/ /;
	$value =~ s/%(..)/pack("C", hex($1))/eg;
	$FORM{$name} = $value;
    }
    $dir = $FORM{dir};


$server  = $ENV{'SERVER_NAME'};
$dirMC  = "/nfs/madgraph/AMCATNLO/";
$uname = $ENV{'REMOTE_USER'};
$webfolder = "http:\/\/$server\/AMCATNLO\/AMCATNLOdata\/$ENV{'REMOTE_USER'}\/$dir";

chdir("$dirMC/$uname/$dir");

	if (open FILE, "process.dat") {
             @incard=<FILE>;
             close (FILE); 
             $process=$incard[0];
	} else {
		$process='Process';
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


	if (open FILE, "res_1_tot.txt") {
             @incard=<FILE>;
             close (FILE); 
             $line=$incard[$#incard];
             $status="Final result";
	} elsif (open FILE, "res_0_tot.txt") {
             @incard=<FILE>;
             close (FILE); 
             $line=$incard[$#incard];
             $status="After grid adjustment";
	} else {
		$line='NotFound';
		if (open FILE, "status.dat") {
             		@incard=<FILE>;
             		close (FILE); 
                	$status=$incard[0];
		}
                else{
			$status = "?"
		} 
	}
        @div_line=split(' ',$line);
# EXAMPLE OF FINAL LINE: Total:  0.21061855E+05 0.18827362E+02   0.08939 %
        $xsect = $div_line[1];
        $xsect_err = $div_line[2];
        $xsect_percent = $div_line[3];

print "Content-type:text/html\r\n\r\n";
print "<html>";
print "<head>";
print "<title></title>";
print "<meta http-equiv=\"refresh\" content=\"30\">";
print "</head>";
print "<body>";
print "<font  color=\"#AA0000\" face=\"Courier\"><br><br> ";
print "Information for process <b>$process</b> @ $coll_id:<br><br><hr></font> ";
print "      <font  color=\"#000000\" face=\"Courier\"><p style=\"line-height:150%;\"><p>";
print "<table align=center border=\"2\" bordercolor=\"#009900\" bordercolordark=\"#009900\" cellspacing=\"0\" cellpadding=\"9\" noshade>";
print "<tr> <td bgcolor=\"#009900\" align=center><font color=\"#FFFFFF\"> Total <br>cross-section: </font></td>";
if ($status ne "Setting grid" && substr($status,0,9) ne "Compiling"){
print "<td align=center> $xsect +- $xsect_err <br> ($xsect_percent%) <br> -$status-</td></tr>";
}
else {
print "<td align=center> -$status-</td></tr>";
}
print "<tr> <td bgcolor=\"#009900\" align=center> <font color=\"#FFFFFF\"> Cards: </font></td>";
print "<td align=center><a href=\"$webfolder/param_card.dat\">param_card</a> &nbsp;";
print "<a href=\"$webfolder/run_card.dat\">run_card</a></font></td></tr>";
print "<tr><td bgcolor=\"#009900\" align=center> <font color=\"#FFFFFF\" face=\"Courier\">Events:</font></td>";

if ( -e "allevents.tar.gz"){
print "<td align=center><font face=\"Courier\"><a href=\"$webfolder/allevents.tar.gz\">Download LHE event file</a> </font></td></tr>";
}
else {
print "<td align=center><font face=\"Courier\">Event file not yet available, please wait...</font></td></tr>";
}
print " </center></table>";
print "</body>";
print "</html>";
