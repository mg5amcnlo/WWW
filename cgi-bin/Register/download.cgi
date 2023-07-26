#!/usr/bin/perl

$MG5Version_file = $ENV{'MADGRAPH_BASE'}."/MG5/VERSION";
$MG5Version_file_v3 = $ENV{'MADGRAPH_BASE'}."/version3/VERSION";
$PyPGSVersion_file = $ENV{'MADGRAPH_BASE'}."/MG_ME/pythia-pgs/PyPGSVersion.txt";
$IntVersion_file = $ENV{'MADGRAPH_BASE'}."/WWW/htdocs/Downloads/interfaces/InterfaceVersion.txt";
$ExRootVersion_file = $ENV{'MADGRAPH_BASE'}."/MG_ME/ExRootAnalysis/ExRootVersion.txt";
$TDPLOTSVersion_file = $ENV{'MADGRAPH_BASE'}."/MG_ME/MadAnalysis/MAVersion.txt";
$ECVersion_file = $ENV{'MADGRAPH_BASE'}."/MG_ME/EventConverter/ECVersion.txt";
$MWVersion_file = $ENV{'MADGRAPH_BASE'}."/MG_ME/Template/Source/MadWeight_File/MW_TemplateVersion.txt";
$MGMEVersion_file = $ENV{'MADGRAPH_BASE'}."/MG_ME/MGMEVersion.txt";

open (MG5FILE,$MG5Version_file) || die "Error reading $MG5Version_file";
@temp=<MG5FILE>;
$MG5Version=$temp[0];
$MG5Version =~ /^version\s+=\s+(.+)$/;
$MG5Version = $1;
$MG5Version =~ /^(.+)\.\d+$/;
$MG5MILESTONEVersion = $1;
my $c = $MG5MILESTONEVersion =~ tr/\.//;
while ($c > 1){
	$MG5MILESTONEVersion =~ /^(.+)\.\d+$/;
	$MG5MILESTONEVersion = $1;	
	$c--;
}

open (MG5FILE,$MG5Version_file_v3) || die "Error reading $MG5Version_file_v3";
@temp=<MG5FILE>;
$MG5Versionv3=$temp[0];
$MG5Versionv3 =~ /^version\s+=\s+(.+)$/;
$MG5Versionv3 = $1;
$MG5Versionv3 =~ /^(.+)\.\d+$/;
$MG5MILESTONEVersionv3 = $1;
my $c = $MG5MILESTONEVersionv3 =~ tr/\.//;
while ($c > 1){
	$MG5MILESTONEVersionv3 =~ /^(.+)\.\d+$/;
	$MG5MILESTONEVersionv3 = $1;	
	$c--;
} 

close(MG5FILE);

#open (MGMEFILE,$MGMEVersion_file) || die "Error reading $MGMEVersion_file";
#@temp=<MGMEFILE>;
#$MGMEVersion=$temp[0];

#close(MGMEFILE);
$MGMEVersion='4.5.2';
chomp($MGMEVersion);

#open (PYPGSFILE,$PyPGSVersion_file) || die "Error reading $PyPGSVersion_file";
#@temp=<PYPGSFILE>;
$PyPGSVersion='2.4.5';
#chomp($PyPGSVersion);
#close(PYPGSFILE);

#open (INTFILE,$IntVersion_file) || die "Error reading $IntVersion_file";
#@temp=<INTFILE>;
$IntVersion='3.0.1';
chomp($IntVersion);
#close(INTFILE);

#open (EXROOTFILE,$ExRootVersion_file) || die "Error reading $ExRootVersion_file";
#@temp=<EXROOTFILE>;
$ExRootVersion='1.1.2';
chomp($ExRootVersion);
#close(EXROOTFILE);

#open (TDPLOTSFILE,$TDPLOTSVersion_file) || die "Error reading $TDPLOTSVersion_file";
#@temp=<TDPLOTSFILE>;
#$MAVersion=$temp[0];
#chomp($TDPLOTSVersion);
#close(TDPLOTSFILE);

#open (ECFILE,$ECVersion_file) || die "Error reading $ECVersion_file";
#@temp=<ECFILE>;
#$ECVersion=$temp[0];
#chomp($ECVersion);
#close(ECFILE);

#open (MWFILE,$MWVersion_file) || die "Error reading $MWVersion_file";
#@temp=<MWFILE>;
$MWVersion="2.5.16";
chomp($MWVersion);
#close(MWFILE);

print "Content-type: text/html\n\n";
print "
<html>
<head>
   <meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">
   <title>Downloads</title>
</head>
<body text=\"#000000\" bgcolor=\"#FFFFFF\" link=\"#0000FF\" vlink=\"#800080\" alink=\"#FF0000\">

<dir>
<center><font color=\"#008080\"><font size=+3>&nbsp;Downloads</font></font>
<br><font color=\"#FF00FF\">(To download these files.tar.gz, hold down the
shift key while you click on the link)</font></center>

<p><br>
<table BORDER WIDTH=\"100%\" >
<tr>
<td>
<center><font size=+1>CODE</font></center>
</td>

<td>
<center><font size=+1><font color=\"#008080\">&nbsp;</font><font color=\"#000000\">DESCRIPTION</font></font></center>
</td>
</tr>


<tr>
<td>
<center><font size=+2><a href=\"https://launchpad.net/mg5amcnlo/lts/2.9.x/+download/MG5_aMC_v$MG5Version.tar.gz\">MadGraph5_aMC\@NLO</a><br> Long Term Stable Version</font></center>
</td>

<td><font size=+1>
<FONT COLOR=\"#990099\"> Long Term Stable Version.</font>
The long term stable version of the MadGraph5_aMC@NLO matrix element generator package.<br>
Please note that you need Python v 2.7 or 3.7 (and higher) to use MadGraph5_aMC@NLO.
Untar, and start the user interface by running<br>
<FONT COLOR=\"#00aa00\">./bin/mg5_amc</font><br>
Then type<br>
<FONT COLOR=\"#00aa00\">tutorial</font>
<br>to start a short tutorial how to use the program.<br>
<!-- Upon request, MadGraph5_aMC@NLO creates the code of the corresponding
process(es) generating the amplitudes for all the relevant
subprocesses and the mappings for the integration over the phase
space. A process-dependent stand-alone code is produced from a
template that allows the user to calculate cross sections and to
obtain unweighted events. Once the events have been generated they may
be passed to any shower Monte Carlo program (such as
<a href=\"http://hepwww.rl.ac.uk/theory/seymour/herwig\" target=\"_top\">HERWIG</a> or 
<a href=\"http://www.thep.lu.se/~torbjorn/Pythia.html\"  target=\"_top\">PYTHIA</a>)
where partons are perturbatively evolved through the emission of QCD
radiation, and eventually turned into physical states (hadronization).<br>
Standalone matrix element output in Fortran and C++ is also available, as well as output of matrix element libraries for <a href=\"http://www.thep.lu.se/~torbjorn/Pythia.html\"  target=\"_top\">Pythia 8</a>.
<br><br> -->
See the <a href=\"https://launchpad.net/madgraph5\">MadGraph5_aMC\@NLO Launchpad page</a> for more info about MadGraph5_aMC@NLO.
<a href=\"/Downloads/UpdateNotes_mg5.txt\">Update notes</a>.
</font><p>
</td>
</tr>

<tr>
<td>
<center><font size=+2><a href=\"http://launchpad.net/madgraph5/3.0/$MG5MILESTONEVersionv3.x/+download/MG5_aMC_v$MG5Versionv3.tar.gz\">MadGraph5_aMC\@NLO</a><br> Latest Feature Release<br> (include electroweak)</font></center>
</td>

<td><font size=+1>
<FONT COLOR=\"#990099\"> Developer's kit.</font>
The latest version of the MadGraph5_aMC@NLO matrix element generator package.<br>
Please note that you need Python v3.7 (and higher) to use MadGraph5_aMC@NLO.
Untar, and start the user interface by running<br>
<FONT COLOR=\"#00aa00\">./bin/mg5_amc</font><br>
Then type<br>
<FONT COLOR=\"#00aa00\">tutorial</font>
<br>to start a short tutorial how to use the program.<br>
<!-- Upon request, MadGraph5_aMC@NLO creates the code of the corresponding
process(es) generating the amplitudes for all the relevant
subprocesses and the mappings for the integration over the phase
space. A process-dependent stand-alone code is produced from a
template that allows the user to calculate cross sections and to
obtain unweighted events. Once the events have been generated they may
be passed to any shower Monte Carlo program (such as
<a href=\"http://hepwww.rl.ac.uk/theory/seymour/herwig\" target=\"_top\">HERWIG</a> or
<a href=\"http://www.thep.lu.se/~torbjorn/Pythia.html\"  target=\"_top\">PYTHIA</a>)
where partons are perturbatively evolved through the emission of QCD
radiation, and eventually turned into physical states (hadronization).<br>
Standalone matrix element output in Fortran and C++ is also available, as well as output of matrix element libraries for <a href=\"ht\
tp://www.thep.lu.se/~torbjorn/Pythia.html\"  target=\"_top\">Pythia 8</a>.
<br><br> -->
See the <a href=\"https://launchpad.net/madgraph5\">MadGraph5_aMC\@NLO Launchpad page</a> for more info about MadGraph5_aMC@NLO.
<a href=\"/Downloads/UpdateNotes_mg5.txt\">Update notes</a>.
</font><p>
</td>
</tr>
</table >
<center><font color=\"#008080\"><font size=+3>&nbsp;Legacy code</font></font></center><br>

<table BORDER WIDTH=\"100%\" >
<tr>
<td>
<center><font size=+2><a href=\"/Downloads/MG_ME_DIP_V$MGMEVersion.tar.gz\">MadDipole</a></font></center>
</td>
<td><font size=+1>
<FONT COLOR=\"#990099\"> MadDipole package</font>
Similar to the MadGraph StandAlone package, but particularly useful in the context of Next-to-Leading Order (in QCD or QED) calculations. Given the m+1 particle process MadDipole creates the corresponding code for the matrix element squared and the dipole subtraction terms in the Catani-Seymour framework. Precise details on how to run the code can be found in the MG/ME <a href=\"https://server06.fynu.ucl.ac.be/projects/madgraph/wiki/MadDipole\">wiki page</a> or the README file. By Nicolas Greiner (QCD & QED dipoles) and Rikkert Frederix (QCD dipoles).
</td>
</tr>

<tr>
<td>
<center><font size=+2>
  <a href=\"/Downloads/pythia-pgs_V$PyPGSVersion.tar.gz\">Pythia and PGS package</a>
  </font></center>
</td>

<td><font size=+1> <FONT COLOR=\"#990099\"> Parton showering,
hadronization and detector simulation.</font> This package includes
Pythia 6.420, PGS4 (090401), StdHEP, LHAPDF and Tauola. To automatically run Pythia and PGS on events from the
downloaded MadEvent code, download this package and un-tar it in the
MG_ME_V4.0 directory, then run make in the pythia-pgs
directory. If pythia_card.dat and pgs_card.dat are present in the Cards/ directory,
Pythia and PGS will automatically be called in the event
generation by the MadEvent script generate_events. 
<a href=\"/Downloads/UpdateNotes-pythia-pgs.txt\">Update notes</a> for the Pythia-PGS package.
<br> Note this package can be install automatically in MG5 by the following command:<br>
<FONT COLOR=\"#00aa00\">mg5> install pythia-pgs</font><br>
<font color='red'> Note that this version is not maintained anymore. (use \"install pythia8\" instead)</font><br> 
</td> </tr>

<!--
#<tr>
#<td>
#<center><font size=+2>
#  <a href=\"/Downloads/interfaces2ME_V$IntVersion.tar.gz\">MC interfaces</a>
#  </font></center>
#</td>
#
#<td><font size=+1>
#<FONT COLOR=\"#990099\"> Experimental event simulation.</font>
#  Interface (<a href=\"/Downloads/interfaces/ME2pythia.f\">ME2pythia.f</a>)
# and and sample code (<a href=\"/Downloads/interfaces/main_pythia.f\">main_pythia.f</a>)
#  to read our event
#  files into Pythia and to generate fully showered and hadronized events.
#  By <a href=\"http://home.fnal.gov/~mrenna/\">Steve Mrenna</a> and
#     <a href=\"http://www.isv.uu.se/~alwall/\">Johan Alwall</a>.
#  Interface (<a href=\"/Downloads/interfaces/ME2herwig.f\">ME2herwig.f</a>)
#  and and sample code 
#  (<a href=\"/Downloads/interfaces/main_herwig.f\">main_herwig.f</a>)
#  to read our event files into HERWIG. 
#  By <a href=\"http://www.hep.phy.cam.ac.uk/~richardn/\">Peter Richardson</a> 
#  and Johan Alwall.
#</td>
#</tr>
#<tr>
-->

<tr>
<td>
<center><font size=+2>
  <a href=\"/Downloads/ExRootAnalysis/ExRootAnalysis_V$ExRootVersion.tar.gz\">ExRootAnalysis</a>
  </font></center>
</td>

<td><font size=+1><FONT COLOR=\"#990099\"> Root library.</font> Library
  to analyse the root files created by the PGS run, containing all
  event information: parton level, Pythia event record and PGS
  detector simulation data. See the <a
  href=\"/Downloads/ExRootAnalysis/README\">README file</a> and <a
  href=\"/Downloads/ExRootAnalysis/RootTreeDescription.html\">Root tree
  description</a>, found in the doc directory. Created by Pavel Demin.
<br> Note this package can be install automatically in MG5 by the following command:<br>
<FONT COLOR=\"#00aa00\">mg5> install ExRootAnalysis</font><br> 
  </td> </tr> <tr>

<tr>
<td>
<center><font size=+2>
  <a href=\"/Downloads/MadAnalysis_V$MAVersion.tar.gz\">MadAnalysis</a>
  </font></center>
</td>

<td><font size=+1><FONT COLOR=\"#990099\"> Topdrawer Plotting library.</font> Fortran and Perl 
software to create histograms of kinematic quantities (pt,eta,DeltaR,invariant mass,...) from <br>
<b> 1. Les Houches events </b> <br>
<b> 2. LHC Olympics 4 events </b> <br>
in Topdrawer format (ASCII files, similar to those used in gnuplot).  Topdrawer is a SLAC software. For more information and available downloads see the <a href=\"https://server06.fynu.ucl.ac.be/projects/madgraph/wiki/TopDrawer\">wiki page</a>. Created by F. Maltoni and R. Frederix.
<br> Note this package can be install automatically in MG5 by the following command (This also install td):<br>
<FONT COLOR=\"#00aa00\">mg5> install MadAnalysis4</font><br>
<font color='red'> Note that this version is not maintained anymore. Prefer MadAnalysis5 (install MadAnalysis)</font><br> 
  </td> </tr> 

<!--
#<tr>
#<td>
#<center><font size=+2>
#  <a href=\"https://server06.fynu.ucl.ac.be/projects/delphes/raw-attachment/wiki/WikiStart/Delphes_V_2.0.0.tar.gz\">Delphes</a>
#  </font></center>
#</td>
#
#<td><font size=+1><FONT COLOR=\"#990099\">Detector simulation</font> Delphes is a framework for the fast-simulation of a generic experiment at a high-energy collider, like ATLAS or CMS at the LHC.
#It outputs observable analysis data objects, like missing transverse energy and collections of electrons or jets. The simulation of detector response takes into account the detector resolution, and usual reconstruction algorithms for complex objects, like FastJet.
#A trigger emulation is also performed on its output data. More information available <a href=\"http://www.fynu.ucl.ac.be/users/s.ovyn/Delphes/index.html\">here</a>. Created by S. Ovyn and X. Rouby.
#<br> Note this package can be install automatically in MG5 by the following command:<br>
#<FONT COLOR=\"#00aa00\">mg5> install Delphes</font><br> 
#  </td> </tr>

#<tr>
#<td>
#<center><font size=+2 color=\"#990099\">
#  Misc. Tools
#  </font></center>
#</td>
#
#<td><font size=+1><a href=\"/Downloads/EventConverter_V$ECVersion.tar.gz\">EventConverter</a>. 
#Tool to convert event files between MadEvent v.3 and v.4 (and vice versa). Download, untar and run \"make\". Written by J. Alwall.
#<br>
#<a href=\"/Downloads/Calculators.tar.gz\">Calculators</a>.
#Source code for all calculators available online. Each calculator can be compiled with \"make\".
#<br>
#</font>
#</td> </tr> 
-->

<tr>
<td>
<center><font size=+2><a href=\"/Downloads/MG_ME_V$MGMEVersion.tar.gz\">MadGraph V4</a></font></center>
</td>

<td><font size=+1>
This is the old version of MG/ME, retained to allow comparisons with the latest version.<br>
<font color='red'> Note that this version is not maintained anymore.</font><br> 
<a href=\"/Downloads/UpdateNotes.txt\">Update notes</a> for MadGraph/MadEvent 4.
</font></font>
</td>
</tr>

<tr>
<td>
<center><font size=+2><a href=\"/Downloads/MG_ME_MW_V$MWVersion.tar.gz\">MadWeight V2</a></font></center>
</td>

<td><font size=+1>
<FONT COLOR=\"#990099\"> MadWeight2 package</font>
The MadWeight package is a specific phase space generator designed for the Matrix Element Reweighting. MadWeight computes the convolution of the squared matrix element and the resolution function for a sample of events and a number of theoretical hypothesis. This gives a usefull discriminator between different theoretical inputs. Precise details on how to run the code can be found in the MG/ME <a href=\"https://server06.fynu.ucl.ac.be/projects/madgraph/wiki/MadWeight\">wiki page</a>. By Olivier Mattelaer and Pierre Artoisenet.
<font color='red'> Note that this version is not maintained anymore.</font><br>
The most recent version of MadWeight is now part of the MadGraph5_aMC@NLO framework.
</td>
</tr>


<!--
<tr>
<td>
<center><font size=+2><a href=\"/Downloads/Ngluons.tar.gz\">Ngluons</a></font></center>
</td>


<td><font size=+1>
<FONT COLOR=\"#990099\"> For twistor addicts.</font>   
  Sample code for calculating all-gluon partonic cross sections gg->(N-2)g, with N<=12
  (N=12 is the current
  Guinness World's record!!), using the good-old
  Berends-Giele recursive relations. It uses the HELAS library (included) and is based on the color-flow decomposition as described in <a href=\"http://arXiv.org/abs/hep-ph/0209271\"> hep-ph/0209271</a>. 
</td>
</tr>
-->


</table>
</dir>

<br>&nbsp;
<br>&nbsp;
</body>
</html>";

