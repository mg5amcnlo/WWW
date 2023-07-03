#!/usr/bin/perl

#############################################################
#                                                          ##
#                   TwoMadGraph Project                    ##
#                                                          ##
# FILE : TwoHiggsCalcsb.cgi                                ##
# VERSION : 1.1                                            ##
# DATE : 8 March 2006                                      ##
# AUTHOR : M. Herquet (UCL-CP3)                            ##
#                                                          ##
# DESCRIPTION : script to create a new file from web form  ##
# and run TwoHiggsCalc on it                               ##
#############################################################

$uname  = $ENV{REMOTE_USER};
$dirMAD = $ENV{'MADGRAPH_DATA'};
$server = $ENV{'SERVER_NAME'};
$dirHTML= "MadGraphData";

# HTML header
print "Content-type: text/html\n\n"; 
print "<HTML>\n";
print "<BODY BGCOLOR=#FFFFFF>\n";

# Read data from the html form 
read(STDIN,$temp,$ENV{'CONTENT_LENGTH'});

# Store it into the table field
@pairs=split(/&/,$temp);
foreach $item(@pairs)
{
  ($key,$content)=split(/=/,$item,2);
  $fields{$key}=$content;
}                            

# Convert algebraic expressions 
while ( my ($key, $value) = each(%fields) ) {

    $value =~ s/PI/3.141592653589793238/g;
    $value =~ s/\%2F/\//g;
    $value =~ s/\%2B/\+/g;
#    $value =~ s/v/(eval($fields{v}))/g;
    $fields{$key}=eval($value);
}

# call from 
$callfrom  = $fields{callfrom};
# directory name
$procname  = $fields{procname};
# here the web_sm_calc-pl is called as a standalone
if( $callfrom eq "standalone"){
	$procname='';	
       $directory="$dirMAD/$uname";
       # check if the Cards directory exists
       if(! (-d  "$dirMAD/$uname/Cards")){system(" mkdir $dirMAD/$uname/Cards")}
       if(! (-d  "$dirMAD/$uname/Cards/THCFiles")){system(" mkdir $dirMAD/$uname/Cards/THCFiles")}
       $temp_id=0;
       $paramcard="param_card_$temp_id.dat";
       while (-e "$dirMAD/$uname/Cards/$paramcard"){
       $temp_id++;
       $paramcard="param_card_$temp_id.dat";
       }
       
}
# here the web_sm_calc-pl is called in setting up the web generation
 elsif( $callfrom eq "webgeneration"){
 if(! (-d  "$dirMAD/$uname/Cards")){system(" mkdir $dirMAD/$uname/Cards")}
 if(! (-d  "$dirMAD/$uname/Cards/THCFiles")){system(" mkdir $dirMAD/$uname/Cards/THCFiles")}
 $directory = "$dirMAD/$uname/$procname";
 $paramcard="param_card.dat";
 }

if ( $fields{basis} eq "generic") {
# Write it in a external file
$base_adr = "$dirMAD/$uname/Cards/THCFiles/";
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)=localtime(time);
$time_tag=sprintf("%4d%02d%02d%02d%02d%02d",$year+1900,$mon+1,$mday,$hour,$min,$sec);
$adr=$base_adr.'sbinput'.$time_tag.'.txt';

open(myoutfile,">$adr") || print "Can't open parameter file for writing";

print myoutfile "# Input file for Gen2HB generated from the web interface\n";

print myoutfile "Block MODSEL # Select Model\n";
print myoutfile "     1   10    # 10=2HDM\n";

print myoutfile "Block SMINPUTS      # Standard Model inputs\n";
print myoutfile sprintf("     1      % 16.8e   # alpha_em(MZ)(-1) SM MSbar\n",$fields{invalpha});
print myoutfile sprintf("     2      % 16.8e   # G_Fermi\n",$fields{G_Fermi}*1e-5);
print myoutfile sprintf("     3      % 16.8e   # alpha_s(MZ) SM MSbar\n",$fields{alpha_s});
print myoutfile sprintf("     4      % 16.8e   # MZ(Pole)\n",$fields{ZMASS});

# print myoutfile "Block MGSMPARAM   # Additional SM param needed\n";

print myoutfile "Block MGYUKAWA # Yukawa masses\n";
print myoutfile sprintf("     3      % 16.8e   # Ms MSbar\n",$fields{MSSMASS});
print myoutfile sprintf("     5      % 16.8e   # Mb MSbar\n",$fields{MSBMASS});
print myoutfile sprintf("     4      % 16.8e   # Mc MSbar\n",$fields{MSCMASS});
print myoutfile sprintf("     6      % 16.8e   # Mt MSbar\n",$fields{MSTMASS});
print myoutfile sprintf("    13      % 16.8e   # Mmu MSbar\n",$fields{MSMUMASS});
print myoutfile sprintf("    15      % 16.8e   # Mtau MSbar\n",$fields{MSTAMASS});

print myoutfile "Block MGCKM  # CKM matrix\n";
print myoutfile sprintf("    1   1      % 16.8e   # Vud\n",sqrt(1-$fields{Vus}**2));
print myoutfile sprintf("    1   2      % 16.8e   # Vus\n",$fields{Vus});
print myoutfile sprintf("    1   3      % 16.8e   # Vub\n",0);
print myoutfile sprintf("    2   1      % 16.8e   # Vcd\n",$fields{Vus});
print myoutfile sprintf("    2   2      % 16.8e   # Vcs\n",sqrt(1-$fields{Vus}**2));
print myoutfile sprintf("    2   3      % 16.8e   # Vcb\n",0);
print myoutfile sprintf("    3   1      % 16.8e   # Vtd\n",0);
print myoutfile sprintf("    3   2      % 16.8e   # Vts\n",0);
print myoutfile sprintf("    3   3      % 16.8e   # Vtb\n",1);

print myoutfile "Block BASIS  # Basis convention choice\n";
print myoutfile "     1   2   # Standard Basis\n";

print myoutfile "Block MINPAR  # Minimal parameter set\n";
print myoutfile sprintf("     1   1     % 16.8e   # lambda1, coeff of 1111\n",$fields{sblambda1});
print myoutfile sprintf("     2   1     % 16.8e   # lambda2, coeff of 2222\n",$fields{sblambda2});
print myoutfile sprintf("     3   1     % 16.8e   # lambda3, coeff of 1122\n",$fields{sblambda3});
print myoutfile sprintf("     4   1     % 16.8e   # lambda4, coeff of 1221\n",$fields{sblambda4});
print myoutfile sprintf("     5   1     % 16.8e   # Re part of lambda5, coeff of 1212\n",$fields{sblambda5}*cos($fields{sbphase_l5}));
print myoutfile sprintf("     5   2     % 16.8e   # Im part of lambda5, coeff of 1212\n",$fields{sblambda5}*sin($fields{sbphase_l5}));
print myoutfile sprintf("     6   1     % 16.8e   # Re part of lambda6, coeff of 1112\n",$fields{sblambda6}*cos($fields{sbphase_l6}));
print myoutfile sprintf("     6   2     % 16.8e   # Im part of lambda6, coeff of 1112\n",$fields{sblambda6}*sin($fields{sbphase_l6}));
print myoutfile sprintf("     7   1     % 16.8e   # Re part of lambda7, coeff of 2212\n",$fields{sblambda7}*cos($fields{sbphase_l7}));
print myoutfile sprintf("     7   2     % 16.8e   # Im part of lambda7, coeff of 2212\n",$fields{sblambda7}*sin($fields{sbphase_l7}));
print myoutfile sprintf("     8   1     % 16.8e   # Tan(beta)\n",$fields{tanb});
print myoutfile sprintf("     9   1     % 16.8e   # Phase of v2\n",$fields{xi});
print myoutfile sprintf("    10   1     % 16.8e   # Norm of mu3\n",$fields{mu3});

print myoutfile "Block YUKAWA2  # Yukawa couplings of second doublet in the generic basis\n"; 
print myoutfile sprintf("     1   1   1     % 16.8e   # G_1D Real part\n",$fields{Y1D}*cos($fields{phase_Y1D})); 
print myoutfile sprintf("     1   1   2     % 16.8e   # G_1D Imaginary part\n",$fields{Y1D}*sin($fields{phase_Y1D})); 
print myoutfile sprintf("     1   2   1     % 16.8e   # G_2D Real part\n",$fields{Y2D}*cos($fields{phase_Y2D}));  
print myoutfile sprintf("     1   2   2     % 16.8e   # G_2D Imaginary part\n",$fields{Y2D}*sin($fields{phase_Y2D}));  
print myoutfile sprintf("     1   3   1     % 16.8e   # G_3D Real part\n",$fields{Y3D}*cos($fields{phase_Y3D}));  
print myoutfile sprintf("     1   3   2     % 16.8e   # G_3D Imaginary part\n",$fields{Y3D}*sin($fields{phase_Y3D}));  
print myoutfile sprintf("     2   1   1     % 16.8e   # G_1S Real part\n",$fields{Y1S}*cos($fields{phase_Y1S}));  
print myoutfile sprintf("     2   1   2     % 16.8e   # G_1S Imaginary part\n",$fields{Y1S}*sin($fields{phase_Y1S}));  
print myoutfile sprintf("     2   2   1     % 16.8e   # G_2S Real part\n",$fields{Y2S}*cos($fields{phase_Y2S}));  
print myoutfile sprintf("     2   2   2     % 16.8e   # G_2S Imaginary part\n",$fields{Y2S}*sin($fields{phase_Y2S}));  
print myoutfile sprintf("     2   3   1     % 16.8e   # G_3S Real part\n",$fields{Y3S}*cos($fields{phase_Y3S}));  
print myoutfile sprintf("     2   3   2     % 16.8e   # G_3S Imaginary part\n",$fields{Y3S}*sin($fields{phase_Y3S}));  
print myoutfile sprintf("     3   1   1     % 16.8e   # G_1B Real part\n",$fields{Y1B}*cos($fields{phase_Y1B}));  
print myoutfile sprintf("     3   1   2     % 16.8e   # G_1B Imaginary part\n",$fields{Y1B}*sin($fields{phase_Y1B}));  
print myoutfile sprintf("     3   2   1     % 16.8e   # G_2B Real part\n",$fields{Y2B}*cos($fields{phase_Y2B}));  
print myoutfile sprintf("     3   2   2     % 16.8e   # G_2B Imaginary part\n",$fields{Y2B}*sin($fields{phase_Y2B}));  
print myoutfile sprintf("     3   3   1     % 16.8e   # G_3B Real part\n",$fields{Y3B}*cos($fields{phase_Y3B}));  
print myoutfile sprintf("     3   3   2     % 16.8e   # G_3B Imaginary part\n",$fields{Y3B}*sin($fields{phase_Y3B}));
  
print myoutfile sprintf("     4   1   1     % 16.8e   # G_1U Real part\n",$fields{Y1U}*cos($fields{phase_Y1U}));  
print myoutfile sprintf("     4   1   2     % 16.8e   # G_1U Imaginary part\n",$fields{Y1U}*sin($fields{phase_Y1U}));  
print myoutfile sprintf("     4   2   1     % 16.8e   # G_2U Real part\n",$fields{Y2U}*cos($fields{phase_Y2U}));  
print myoutfile sprintf("     4   2   2     % 16.8e   # G_2U Imaginary part\n",$fields{Y2U}*sin($fields{phase_Y2U}));  
print myoutfile sprintf("     4   3   1     % 16.8e   # G_3U Real part\n",$fields{Y3U}*cos($fields{phase_Y3U}));  
print myoutfile sprintf("     4   3   2     % 16.8e   # G_3U Imaginary part\n",$fields{Y3U}*sin($fields{phase_Y3U}));  
print myoutfile sprintf("     5   1   1     % 16.8e   # G_1C Real part\n",$fields{Y1C}*cos($fields{phase_Y1C}));  
print myoutfile sprintf("     5   1   2     % 16.8e   # G_1C Imaginary part\n",$fields{Y1C}*sin($fields{phase_Y1C}));  
print myoutfile sprintf("     5   2   1     % 16.8e   # G_2C Real part\n",$fields{Y2C}*cos($fields{phase_Y2C}));  
print myoutfile sprintf("     5   2   2     % 16.8e   # G_2C Imaginary part\n",$fields{Y2C}*sin($fields{phase_Y2C}));  
print myoutfile sprintf("     5   3   1     % 16.8e   # G_3C Real part\n",$fields{Y3C}*cos($fields{phase_Y3C}));  
print myoutfile sprintf("     5   3   2     % 16.8e   # G_3C Imaginary part\n",$fields{Y3C}*sin($fields{phase_Y3C}));  
print myoutfile sprintf("     6   1   1     % 16.8e   # G_1T Real part\n",$fields{Y1T}*cos($fields{phase_Y1T}));  
print myoutfile sprintf("     6   1   2     % 16.8e   # G_1T Imaginary part\n",$fields{Y1T}*sin($fields{phase_Y1T}));  
print myoutfile sprintf("     6   2   1     % 16.8e   # G_2T Real part\n",$fields{Y2T}*cos($fields{phase_Y2T}));  
print myoutfile sprintf("     6   2   2     % 16.8e   # G_2T Imaginary part\n",$fields{Y2T}*sin($fields{phase_Y2T}));  
print myoutfile sprintf("     6   3   1     % 16.8e   # G_3T Real part\n",$fields{Y3T}*cos($fields{phase_Y3T}));  
print myoutfile sprintf("     6   3   2     % 16.8e   # G_3T Imaginary part\n",$fields{Y3T}*sin($fields{phase_Y3T}));  

print myoutfile sprintf("     7   1   1     % 16.8e   # G_1E Real part\n",$fields{Y1E}*cos($fields{phase_Y1E}));  
print myoutfile sprintf("     7   1   2     % 16.8e   # G_1E Imaginary part\n",$fields{Y1E}*sin($fields{phase_Y1E}));  
print myoutfile sprintf("     7   2   1     % 16.8e   # G_2E Real part\n",$fields{Y2E}*cos($fields{phase_Y2E}));  
print myoutfile sprintf("     7   2   2     % 16.8e   # G_2E Imaginary part\n",$fields{Y2E}*sin($fields{phase_Y2E}));  
print myoutfile sprintf("     7   3   1     % 16.8e   # G_3E Real part\n",$fields{Y3E}*cos($fields{phase_Y3E}));  
print myoutfile sprintf("     7   3   2     % 16.8e   # G_3E Imaginary part\n",$fields{Y3E}*sin($fields{phase_Y3E}));  
print myoutfile sprintf("     8   1   1     % 16.8e   # G_1MU Real part\n",$fields{Y1MU}*cos($fields{phase_Y1MU}));  
print myoutfile sprintf("     8   1   2     % 16.8e   # G_1MU Imaginary part\n",$fields{Y1MU}*sin($fields{phase_Y1MU}));  
print myoutfile sprintf("     8   2   1     % 16.8e   # G_2MU Real part\n",$fields{Y2MU}*cos($fields{phase_Y2MU}));  
print myoutfile sprintf("     8   2   2     % 16.8e   # G_2MU Imaginary part\n",$fields{Y2MU}*sin($fields{phase_Y2MU}));  
print myoutfile sprintf("     8   3   1     % 16.8e   # G_3MU Real part\n",$fields{Y3MU}*cos($fields{phase_Y3MU}));  
print myoutfile sprintf("     8   3   2     % 16.8e   # G_3MU Imaginary part\n",$fields{Y3MU}*sin($fields{phase_Y3MU}));  
print myoutfile sprintf("     9   1   1     % 16.8e   # G_1TA Real part\n",$fields{Y1TA}*cos($fields{phase_Y1TA}));  
print myoutfile sprintf("     9   1   2     % 16.8e   # G_1TA Imaginary part\n",$fields{Y1TA}*sin($fields{phase_Y1TA}));  
print myoutfile sprintf("     9   2   1     % 16.8e   # G_2TA Real part\n",$fields{Y2TA}*cos($fields{phase_Y2TA}));  
print myoutfile sprintf("     9   2   2     % 16.8e   # G_2TA Imaginary part\n",$fields{Y2TA}*sin($fields{phase_Y2TA}));  
print myoutfile sprintf("     9   3   1     % 16.8e   # G_3TA Real part\n",$fields{Y3TA}*cos($fields{phase_Y3TA}));  
print myoutfile sprintf("     9   3   2     % 16.8e   # G_3TA Imaginary part\n",$fields{Y3TA}*sin($fields{phase_Y3TA}));  

print myoutfile "Block MASS  # Masses\n";
print myoutfile sprintf("     3      % 16.8e   # Ms\n",$fields{SMASS});
print myoutfile sprintf("     4      % 16.8e   # Mc\n",$fields{CMASS});
print myoutfile sprintf("     5      % 16.8e   # Mb\n",$fields{BMASS});
print myoutfile sprintf("     6      % 16.8e   # Mt\n",$fields{TMASS});
print myoutfile sprintf("    13      % 16.8e   # Mmu\n",$fields{MUMASS});
print myoutfile sprintf("    15      % 16.8e   # Mta\n",$fields{TAMASS});

close(myoutfile);

# Download link
print "Your generic basis data has been successfully written and can be downloaded <a href=/MadGraphData/$uname/Cards/THCFiles/sbinput$time_tag.txt>here</a><br>";

# Prepare output files for Gen2HB
$adr_outputg2hb=$base_adr.'input'.$time_tag.'.txt';
$adr_logg2hb=$base_adr.'gen2hb'.$time_tag.'.log';
# Build system call string for Gen2HB
@args_gen2hb=("$ENV{'MADGRAPH_BASE'}/MG_ME/WebBin/Calculators/gen2hb", $adr, $adr_outputg2hb,$adr_logg2hb,'1e-10');
# Call Gen2HB
system(@args_gen2hb) == 0 or print "system @args failed: $?";

print "Your data has been sent to gen2hb, ";

# Check in the log if there is no error
open(myinfile,"<$adr_logg2hb") || print "Can't open $adr_logg2hb for reading";
@result=<myinfile>;
$err_tag=0;
foreach $line (@result)
{	
    if (index($line,'ERROR') != -1) {
	print "<pre>";print $line;print "</pre>";
	$err_tag=1;
    }

}
if ( $err_tag != 0 ) 
{
	print "but it did not manage to process your file due to the above errors!<br>";
	if( $callfrom eq "webgeneration"){
    print "<h2><center><a  href=\"http://$server/$dirHTML/$uname/$procname/HTML/web_run_form.html\" >To Go Back to the Web Run Form, Click Here !</a> </center></h2><br>\n";
}
	die;	
}
close(myinfile);

# If not write download link
print "and has been successfully processed, you can download the result <a href=/MadGraphData/$uname/Cards/THCFiles/input$time_tag.txt>here</a><br>";


$adr=$adr_outputg2hb;
$adr2=$base_adr.'output'.$time_tag.'.txt';
$logadr=$base_adr.'twohiggscalc'.$time_tag.'.log';
@args = ("$ENV{'MADGRAPH_BASE'}/MG_ME/WebBin/Calculators/TwoHiggsCalc", $adr, $adr2,$logadr);
system(@args) == 0 or print "system @args failed: $?";

print "Your data has been sent to TwoHiggsCalc, ";

# Check the log file for errors 
open(myinfile,"<$logadr") || print "Can't open $logadr for reading";
@result=<myinfile>;
foreach $line (@result)
{
    if (index($line,'ERROR') != -1) {
	print "but it did not manage to process your file :<br>";
	print $line;
	if( $callfrom eq "webgeneration"){
    print "<h2><center><a  href=\"http://$server/$dirHTML/$uname/$procname/HTML/web_run_form.html\" >To Go Back to the Web Run Form, Click Here !</a> </center></h2><br>\n";
}
	die;
    }

} 
close(myinfile);

system("cp $adr2 $directory/Cards/$paramcard");

# If no errors display download link
print "and has been successfully processed, you can download the result <a href=/MadGraphData/$uname/$procname/Cards/$paramcard>here</a><br>";

if( $callfrom eq "webgeneration"){
    print "This file is now the param_card of your process.<br>";
    print "<h2><center><a  href=\"http://$server/$dirHTML/$uname/$procname/HTML/web_run_form.html\" >To Go Back to the Web Run Form, Click Here !</a> </center></h2><br>\n";
} else {

# Print TwoHiggsCalc output
print "TwoHiggsCalc results :<br>";
print "<br><pre>";
open(myinfile,"<$adr2") || print "Can't open $adr2 for reading";
@result=<myinfile>;
foreach $line (@result)
{
    print $line;
} 
close(myinfile);
}
print "</pre>";
print "</BODY></HTML>";

} else {
# Write it in a external file
$base_adr = "$dirMAD/$uname/Cards/THCFiles/";
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)=localtime(time);
$time_tag=sprintf("%4d%02d%02d%02d%02d%02d",$year+1900,$mon+1,$mday,$hour,$min,$sec);

$adr=$base_adr.'input'.$time_tag.'.txt';

open(myoutfile,">$adr") || print "Can't open parameter file for writing";

print myoutfile "# Input file for TwoHiggsCalc generated from the web interface\n";

print myoutfile "Block MODSEL # Select Model\n";
print myoutfile "     1   10    # 10=2HDM\n";

print myoutfile "Block SMINPUTS      # Standard Model inputs\n";
print myoutfile sprintf("     1      % 16.8e   # alpha_em(MZ)(-1) SM MSbar\n",$fields{invalpha});
print myoutfile sprintf("     2      % 16.8e   # G_Fermi\n",$fields{G_Fermi}*1e-5);
print myoutfile sprintf("     3      % 16.8e   # alpha_s(MZ) SM MSbar\n",$fields{alpha_s});
print myoutfile sprintf("     4      % 16.8e   # MZ(Pole)\n",$fields{ZMASS});

# print myoutfile "Block MGSMPARAM   # Additional SM param needed\n";

print myoutfile "Block MGYUKAWA # Yukawa masses\n";
print myoutfile sprintf("     3      % 16.8e   # Ms MSbar\n",$fields{MSSMASS});
print myoutfile sprintf("     5      % 16.8e   # Mb MSbar\n",$fields{MSBMASS});
print myoutfile sprintf("     4      % 16.8e   # Mc MSbar\n",$fields{MSCMASS});
print myoutfile sprintf("     6      % 16.8e   # Mt MSbar\n",$fields{MSTMASS});
print myoutfile sprintf("    13      % 16.8e   # Mmu MSbar\n",$fields{MSMUMASS});
print myoutfile sprintf("    15      % 16.8e   # Mtau MSbar\n",$fields{MSTAMASS});

print myoutfile "Block MGCKM  # CKM matrix\n";
print myoutfile sprintf("    1   1      % 16.8e   # Vud\n",sqrt(1-$fields{Vus}**2));
print myoutfile sprintf("    1   2      % 16.8e   # Vus\n",$fields{Vus});
print myoutfile sprintf("    1   3      % 16.8e   # Vub\n",0);
print myoutfile sprintf("    2   1      % 16.8e   # Vcd\n",$fields{Vus});
print myoutfile sprintf("    2   2      % 16.8e   # Vcs\n",sqrt(1-$fields{Vus}**2));
print myoutfile sprintf("    2   3      % 16.8e   # Vcb\n",0);
print myoutfile sprintf("    3   1      % 16.8e   # Vtd\n",0);
print myoutfile sprintf("    3   2      % 16.8e   # Vts\n",0);
print myoutfile sprintf("    3   3      % 16.8e   # Vtb\n",1);

print myoutfile "Block BASIS  # Basis convention choice\n";
print myoutfile "     1   1   # Higgs Basis\n";

print myoutfile "Block MINPAR  # Minimal parameter set\n";
print myoutfile sprintf("     1   1     % 16.8e   # lambda1, coeff of 1111\n",$fields{lambda1});
print myoutfile sprintf("     2   1     % 16.8e   # lambda2, coeff of 2222\n",$fields{lambda2});
print myoutfile sprintf("     3   1     % 16.8e   # lambda3, coeff of 1122\n",$fields{lambda3});
print myoutfile sprintf("     4   1     % 16.8e   # lambda4, coeff of 1221\n",$fields{lambda4});
print myoutfile sprintf("     5   1     % 16.8e   # Re part of lambda5, coeff of 1212\n",$fields{lambda5});
print myoutfile sprintf("     5   2     % 16.8e   # Im part of lambda5, coeff of 1212\n",0);
print myoutfile sprintf("     6   1     % 16.8e   # Re part of lambda6, coeff of 1112\n",$fields{lambda6}*cos($fields{phase_l6}));
print myoutfile sprintf("     6   2     % 16.8e   # Im part of lambda6, coeff of 1112\n",$fields{lambda6}*sin($fields{phase_l6}));
print myoutfile sprintf("     7   1     % 16.8e   # Re part of lambda7, coeff of 2212\n",$fields{lambda7}*cos($fields{phase_l7}));
print myoutfile sprintf("     7   2     % 16.8e   # Im part of lambda7, coeff of 2212\n",$fields{lambda7}*sin($fields{phase_l7}));
print myoutfile sprintf("     8   1     % 16.8e   # mass of charged Higgses\n",$fields{mHpm});

print myoutfile "Block YUKAWA2  # Yukawa couplings of second doublet, only in the Higgs basis\n"; 
print myoutfile sprintf("     1   1   1     % 16.8e   # Y_1D Real part\n",$fields{Y1D}*cos($fields{phase_Y1D})); 
print myoutfile sprintf("     1   1   2     % 16.8e   # Y_1D Imaginary part\n",$fields{Y1D}*sin($fields{phase_Y1D})); 
print myoutfile sprintf("     1   2   1     % 16.8e   # Y_2D Real part\n",$fields{Y2D}*cos($fields{phase_Y2D}));  
print myoutfile sprintf("     1   2   2     % 16.8e   # Y_2D Imaginary part\n",$fields{Y2D}*sin($fields{phase_Y2D}));  
print myoutfile sprintf("     1   3   1     % 16.8e   # Y_3D Real part\n",$fields{Y3D}*cos($fields{phase_Y3D}));  
print myoutfile sprintf("     1   3   2     % 16.8e   # Y_3D Imaginary part\n",$fields{Y3D}*sin($fields{phase_Y3D}));  
print myoutfile sprintf("     2   1   1     % 16.8e   # Y_1S Real part\n",$fields{Y1S}*cos($fields{phase_Y1S}));  
print myoutfile sprintf("     2   1   2     % 16.8e   # Y_1S Imaginary part\n",$fields{Y1S}*sin($fields{phase_Y1S}));  
print myoutfile sprintf("     2   2   1     % 16.8e   # Y_2S Real part\n",$fields{Y2S}*cos($fields{phase_Y2S}));  
print myoutfile sprintf("     2   2   2     % 16.8e   # Y_2S Imaginary part\n",$fields{Y2S}*sin($fields{phase_Y2S}));  
print myoutfile sprintf("     2   3   1     % 16.8e   # Y_3S Real part\n",$fields{Y3S}*cos($fields{phase_Y3S}));  
print myoutfile sprintf("     2   3   2     % 16.8e   # Y_3S Imaginary part\n",$fields{Y3S}*sin($fields{phase_Y3S}));  
print myoutfile sprintf("     3   1   1     % 16.8e   # Y_1B Real part\n",$fields{Y1B}*cos($fields{phase_Y1B}));  
print myoutfile sprintf("     3   1   2     % 16.8e   # Y_1B Imaginary part\n",$fields{Y1B}*sin($fields{phase_Y1B}));  
print myoutfile sprintf("     3   2   1     % 16.8e   # Y_2B Real part\n",$fields{Y2B}*cos($fields{phase_Y2B}));  
print myoutfile sprintf("     3   2   2     % 16.8e   # Y_2B Imaginary part\n",$fields{Y2B}*sin($fields{phase_Y2B}));  
print myoutfile sprintf("     3   3   1     % 16.8e   # Y_3B Real part\n",$fields{Y3B}*cos($fields{phase_Y3B}));  
print myoutfile sprintf("     3   3   2     % 16.8e   # Y_3B Imaginary part\n",$fields{Y3B}*sin($fields{phase_Y3B}));
  
print myoutfile sprintf("     4   1   1     % 16.8e   # Y_1U Real part\n",$fields{Y1U}*cos($fields{phase_Y1U}));  
print myoutfile sprintf("     4   1   2     % 16.8e   # Y_1U Imaginary part\n",$fields{Y1U}*sin($fields{phase_Y1U}));  
print myoutfile sprintf("     4   2   1     % 16.8e   # Y_2U Real part\n",$fields{Y2U}*cos($fields{phase_Y2U}));  
print myoutfile sprintf("     4   2   2     % 16.8e   # Y_2U Imaginary part\n",$fields{Y2U}*sin($fields{phase_Y2U}));  
print myoutfile sprintf("     4   3   1     % 16.8e   # Y_3U Real part\n",$fields{Y3U}*cos($fields{phase_Y3U}));  
print myoutfile sprintf("     4   3   2     % 16.8e   # Y_3U Imaginary part\n",$fields{Y3U}*sin($fields{phase_Y3U}));  
print myoutfile sprintf("     5   1   1     % 16.8e   # Y_1C Real part\n",$fields{Y1C}*cos($fields{phase_Y1C}));  
print myoutfile sprintf("     5   1   2     % 16.8e   # Y_1C Imaginary part\n",$fields{Y1C}*sin($fields{phase_Y1C}));  
print myoutfile sprintf("     5   2   1     % 16.8e   # Y_2C Real part\n",$fields{Y2C}*cos($fields{phase_Y2C}));  
print myoutfile sprintf("     5   2   2     % 16.8e   # Y_2C Imaginary part\n",$fields{Y2C}*sin($fields{phase_Y2C}));  
print myoutfile sprintf("     5   3   1     % 16.8e   # Y_3C Real part\n",$fields{Y3C}*cos($fields{phase_Y3C}));  
print myoutfile sprintf("     5   3   2     % 16.8e   # Y_3C Imaginary part\n",$fields{Y3C}*sin($fields{phase_Y3C}));  
print myoutfile sprintf("     6   1   1     % 16.8e   # Y_1T Real part\n",$fields{Y1T}*cos($fields{phase_Y1T}));  
print myoutfile sprintf("     6   1   2     % 16.8e   # Y_1T Imaginary part\n",$fields{Y1T}*sin($fields{phase_Y1T}));  
print myoutfile sprintf("     6   2   1     % 16.8e   # Y_2T Real part\n",$fields{Y2T}*cos($fields{phase_Y2T}));  
print myoutfile sprintf("     6   2   2     % 16.8e   # Y_2T Imaginary part\n",$fields{Y2T}*sin($fields{phase_Y2T}));  
print myoutfile sprintf("     6   3   1     % 16.8e   # Y_3T Real part\n",$fields{Y3T}*cos($fields{phase_Y3T}));  
print myoutfile sprintf("     6   3   2     % 16.8e   # Y_3T Imaginary part\n",$fields{Y3T}*sin($fields{phase_Y3T}));  

print myoutfile sprintf("     7   1   1     % 16.8e   # Y_1E Real part\n",$fields{Y1E}*cos($fields{phase_Y1E}));  
print myoutfile sprintf("     7   1   2     % 16.8e   # Y_1E Imaginary part\n",$fields{Y1E}*sin($fields{phase_Y1E}));  
print myoutfile sprintf("     7   2   1     % 16.8e   # Y_2E Real part\n",$fields{Y2E}*cos($fields{phase_Y2E}));  
print myoutfile sprintf("     7   2   2     % 16.8e   # Y_2E Imaginary part\n",$fields{Y2E}*sin($fields{phase_Y2E}));  
print myoutfile sprintf("     7   3   1     % 16.8e   # Y_3E Real part\n",$fields{Y3E}*cos($fields{phase_Y3E}));  
print myoutfile sprintf("     7   3   2     % 16.8e   # Y_3E Imaginary part\n",$fields{Y3E}*sin($fields{phase_Y3E}));  
print myoutfile sprintf("     8   1   1     % 16.8e   # Y_1MU Real part\n",$fields{Y1MU}*cos($fields{phase_Y1MU}));  
print myoutfile sprintf("     8   1   2     % 16.8e   # Y_1MU Imaginary part\n",$fields{Y1MU}*sin($fields{phase_Y1MU}));  
print myoutfile sprintf("     8   2   1     % 16.8e   # Y_2MU Real part\n",$fields{Y2MU}*cos($fields{phase_Y2MU}));  
print myoutfile sprintf("     8   2   2     % 16.8e   # Y_2MU Imaginary part\n",$fields{Y2MU}*sin($fields{phase_Y2MU}));  
print myoutfile sprintf("     8   3   1     % 16.8e   # Y_3MU Real part\n",$fields{Y3MU}*cos($fields{phase_Y3MU}));  
print myoutfile sprintf("     8   3   2     % 16.8e   # Y_3MU Imaginary part\n",$fields{Y3MU}*sin($fields{phase_Y3MU}));  
print myoutfile sprintf("     9   1   1     % 16.8e   # Y_1TA Real part\n",$fields{Y1TA}*cos($fields{phase_Y1TA}));  
print myoutfile sprintf("     9   1   2     % 16.8e   # Y_1TA Imaginary part\n",$fields{Y1TA}*sin($fields{phase_Y1TA}));  
print myoutfile sprintf("     9   2   1     % 16.8e   # Y_2TA Real part\n",$fields{Y2TA}*cos($fields{phase_Y2TA}));  
print myoutfile sprintf("     9   2   2     % 16.8e   # Y_2TA Imaginary part\n",$fields{Y2TA}*sin($fields{phase_Y2TA}));  
print myoutfile sprintf("     9   3   1     % 16.8e   # Y_3TA Real part\n",$fields{Y3TA}*cos($fields{phase_Y3TA}));  
print myoutfile sprintf("     9   3   2     % 16.8e   # Y_3TA Imaginary part\n",$fields{Y3TA}*sin($fields{phase_Y3TA}));  

print myoutfile "Block MASS  # Masses\n";
print myoutfile sprintf("     3      % 16.8e   # Ms\n",$fields{SMASS});
print myoutfile sprintf("     4      % 16.8e   # Mc\n",$fields{CMASS});
print myoutfile sprintf("     5      % 16.8e   # Mb\n",$fields{BMASS});
print myoutfile sprintf("     6      % 16.8e   # Mt\n",$fields{TMASS});
print myoutfile sprintf("    13      % 16.8e   # Mmu\n",$fields{MUMASS});
print myoutfile sprintf("    15      % 16.8e   # Mta\n",$fields{TAMASS});

close(myoutfile);

print "Your data has been successfully written and can be downloaded <a href=/MadGraphData/$uname/Cards/THCFiles/input$time_tag.txt>here</a><br>";

# Prepare output file
$adr2=$base_adr.'output'.$time_tag.'.txt';
$logadr=$base_adr.'twohiggscalc'.$time_tag.'.log';

# Call TwoHiggsCalc
@args = ("$ENV{'MADGRAPH_BASE'}/MG_ME/WebBin/Calculators/TwoHiggsCalc", $adr, $adr2,$logadr);
system(@args) == 0 or print "system @args failed: $?";
print "Your data has been sent to TwoHiggsCalc, ";

# Check for errors in log file
open(myinfile,"<$logadr") || print "Can't open $logadr for reading";
@result=<myinfile>;
foreach $line (@result)
{
    if (index($line,'ERROR') != -1) {
	print "but it did not manage to process your file :<br>";
	print $line;
	if( $callfrom eq "webgeneration"){
    print "<h2><center><a  href=\"http://$server/$dirHTML/$uname/$procname/HTML/web_run_form.html\" >To Go Back to the Web Run Form, Click Here !</a> </center></h2><br>\n";
}
	die;
    }

} 
close(myinfile);

system("cp $adr2 $directory/Cards/$paramcard");

print "and has been successfully processed, you can download the result <a href=/MadGraphData/$uname/$procname/Cards/$paramcard>here</a><br>";

if( $callfrom eq "webgeneration"){
    print "This file is now the param_card of your process.<br>";
    print "<h2><center><a  href=\"http://$server/$dirHTML/$uname/$procname/HTML/web_run_form.html\" >To Go Back to the Web Run Form, Click Here !</a> </center></h2><br>\n";
} else {

print "TwoHiggsCalc results :<br>";
print "<br><pre>";
open(myinfile,"<$adr2") || print "Can't open $adr2 for reading";
@result=<myinfile>;
foreach $line (@result)
{
    chomp($line);
    print $line."<br>";
} 
close(myinfile);
}
print "</pre>";
print "</BODY></HTML>";
}
