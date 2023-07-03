sub PrintRunForm()
{
print <<ENDPRINT;
Content-type: text/html

<html>
<head>
<title>Generate events online</title>
</head>
<body>
<center><font size=+3>  Web Generation Form </font></center>
<br>
ENDPRINT
if($card_complete ne "") {
    print "<font color=\"red\" size=+1>$card_complete</font><BR>\n";
}
print <<ENDPRINT;
<br>
Please provide the necessary parameter cards for the event generation.<br>
For each card you have the alternatives:
<ul>
<li>Use the present card (default or previously generated/uploaded)
<li>Generate a card using the web form
<li>Upload a card
<li>Use a default card
</ul>

<br>
Or you can also upload a full banner containing all cards (plotting with current plot_card):
<FORM ACTION="http://$server/cgi-bin/RunProcess/webgenerate_events-pl"
      ENCTYPE="multipart/form-data"
      METHOD=POST>
<INPUT TYPE=HIDDEN NAME=directory VALUE="$directory">
<INPUT TYPE=HIDDEN NAME=callfrom  VALUE="banner_upload"> 
<INPUT TYPE="FILE" NAME="banner.txt">
<INPUT TYPE=SUBMIT VALUE="Upload">
</FORM>

<!-- first form -->
<FORM ACTION="http://$server/cgi-bin/RunProcess/webgenerate_events-pl" 
      ENCTYPE="multipart/form-data" 
      METHOD=POST>
<INPUT TYPE=HIDDEN NAME=directory VALUE="$directory"> 
<INPUT TYPE=HIDDEN NAME=callfrom  VALUE="upload"> 
<INPUT TYPE=HIDDEN NAME=run_name  VALUE="$run"> 


<BR>
<center> <INPUT TYPE=SUBMIT VALUE="Send"> the form to generate the cards. </center>
<BR>

<TABLE BORDER=2 align="center" width="90%">  


<!--   MODEL PARAMETERS PART   -->
<tr>
<td>
<center> <font color="#008080"><font size=+2><U><a name=run_form>Model parameters</a></U></font></font></center>

<table border=1 width=90% align=center>
<tr><td>
<table>
<tr><td>
Choose an option for the param_card.dat:
 <select name=param_card>
 <option value=present> Use the present file
 <option value=form> Go to the param_card web form
 <option value=upload> Upload a file
 <option value=default> Use the default file
 </select>
<td>
</td></tr>
<tr><td>
The present <a href="http://$server/$dirHTML/$process/Cards/param_card.dat">param_card.dat</a>
</td></tr>
<tr><td>
The <a href="http://$server/$dirHTML/$process/Cards/param_card_default.dat">default file</a>
</td></tr>
ENDPRINT
if( -e "$directory/HTML/model_calc.html" ) {
    print "<tr><td>\n";
    print "Generate a card at the <a href=\"http://$server/$dirHTML/$process/HTML/model_calc.html\">param_card generation web form</a>\n";
    print "</td></tr>\n";
}
print <<ENDPRINT;
<tr><td>
Upload a param_card: <INPUT TYPE="FILE" NAME="param_card.dat">   <font color="#aa0000">Note! The param_card format for MG5 models differs from the old MG4 models.</font>
</td></tr>
</table>
</td></tr>
</table>
<br>
</td>
</tr>

<!--   COLLIDER AND CUTS PART   -->
<TR valign="top">
  <td align="top">
    <br>
<center> <font color="#008080"><font size=+2><U><a name=run_form>Collider and cuts</a></U></font></font></center>
<p>
<table border=1 width=90% align=center>
<tr><td>
<table>
<tr><td>
Choose an option for the run_card.dat:
 <select name=run_card>
 <option value=present> Use the present file
 <option value=form> Fill in the form below
 <option value=upload> Upload a file
 <option value=default> Use the default file
 </select>
</td></tr>
<tr><td>
The present <a href="http://$server/$dirHTML/$process/Cards/run_card.dat">run_card.dat</a>
<INPUT TYPE=SUBMIT NAME="Edit" VALUE="Edit run_card.dat">
</td></tr>
<tr><td>
The <a href="http://$server/$dirHTML/$process/Cards/run_card_default.dat">default file</a>
</td></tr>
<tr><td>
Upload a run_card: <INPUT TYPE="FILE" NAME="run_card.dat">  </td>
</td></tr>
<tr><td>
Choose the number of sequential runs requested (be carefull if > 1 !):
 <select name=num_run>
 <option value=1> 1
 <option value=2> 2
 <option value=5> 5
 <option value=10> 10
 <option value=20> 20
 </select>
</td></tr>
</table>
</td></tr>
</table>
<BR>
<table>
  <tr>
  <td> Run name </td>
  <td> <input type=text name=run_tag value=fermi size=50> 
    </tr>
   <tr>
    <td>Events requested</td>
    <td>
      <select name=nevents>
	<option value=10000 selected> 10,000
	<option value=25000> 25,000
	<option value=50000> 50,000
      </select>
    </td>
  </tr>  

  <tr>
    <td>Collider</td>
    <td>
      <select name=collider>
	<option value=TeV2 > ppbar @ 1.96 TeV (TeV2)
	<option value=LHC> pp @ 14 TeV (LHC nominal)
	<option value=LHC10 > pp @ 10 TeV (LHC)
	<option value=LHC7 selected> pp @ 7 TeV (LHC Now)
	<option value=NLC > e+e-  @ 500 GeV (ILC)
      </select>
    </td>
  </tr>

  <tr>
    <td>PDF</td>
    <td>
      <select name=pdlabel>
	<option value= cteq6l1 selected>  CTEQ6L1 
	<option value= cteq6_m>           CTEQ6M
	<option value= cteq5_l>           CTEQ5L 
	<option value= mrs02nl>           MRST2002NLO
      </select>
    </td>
  </tr>

  <tr>
    <td>Scales Q=&mu<SUB>ren</SUB> = &mu<SUB>fac</SUB> </td>
    <td>
      <input type=radio name=fixedscales value=F checked> Event-by-event or 
      <input type=radio name=fixedscales value=T> Fixed at
      <input type=text name=scale value=91.188 size=10> GeV
    </td>
  </tr>
  <tr>
    <td>Beam 1 polarization </td>
    <td>
      <input type=text name=polbeam1 value=0 size=10> (in %, between -100 and 100, only for e+/e-)
    </td>
  </tr>
  <tr>
    <td>Beam 2 polarization </td>
    <td>
      <input type=text name=polbeam2 value=0 size=10>
    </td>
  </tr>
</table>
<br>

<br>
<font color="#0000FF"> No Cuts can be set for top,W,Z,H and neutrinos.<BR>
<font color="#0000FF"> Cuts on the minimum transverse momentum, rapidity and energy (in the lab frame):<BR>
<br>
<table>


<TR> <TD>
jets</TD><TD>
: p<SUB>T</SUB>(j) ></TD> <TD>
  <SELECT NAME=ptj>
<OPTION VALUE= 0>no cut
<OPTION VALUE=10>10
<OPTION VALUE=20 SELECTED>20
<OPTION VALUE=30>30
<OPTION VALUE=40>40
<OPTION VALUE=50>50
</SELECT>
</TD> <TD>
&nbsp; |y(j)|< </TD> <TD>
<SELECT NAME=etaj>
<OPTION VALUE=1>1
<OPTION VALUE=1.5>1.5
<OPTION VALUE=2 >2
<OPTION VALUE=2.5 SELECTED>2.5
<OPTION VALUE=3>3
<OPTION VALUE=4>4
<OPTION VALUE=5>5
<OPTION VALUE=6>6
<OPTION VALUE=6.5>6.5
<OPTION VALUE=7>7
<OPTION VALUE=1000>no cut
</SELECT>
</TD> <TD>
&nbsp; E(j) > </TD> <TD>
<input type=text name=ej value=0 size=3> 
</TD></TR>
<TR> <TD>
b</TD><TD>
: p<SUB>T</SUB>(b) >
</TD> <TD>
  <SELECT NAME=ptb>
<OPTION VALUE=0 SELECTED>no cut
<OPTION VALUE=10>10
<OPTION VALUE=20>20
<OPTION VALUE=30>30
<OPTION VALUE=40>40
<OPTION VALUE=50>50
</SELECT>
</TD><TD>
&nbsp; |y(b)|< </TD> <TD>
  <SELECT NAME=etab>
<OPTION VALUE=1>1
<OPTION VALUE=1.5>1.5
<OPTION VALUE=2 >2
<OPTION VALUE=2.5>2.5
<OPTION VALUE=3>3
<OPTION VALUE=4>4
<OPTION VALUE=5>5
<OPTION VALUE=1000 SELECTED>no cut
</SELECT>
</TD> <TD>
&nbsp; E(b) > </TD> <TD>
<input type=text name=eb value=0 size=3> 
</TD></TR>
<TR> <TD>
&#947;</TD><TD>
: p<SUB>T</SUB>(&#947;) ></TD> <TD>
  <SELECT NAME=pta>
<OPTION VALUE= 0>no cut
<OPTION VALUE=10 >10
<OPTION VALUE=20 SELECTED>20
<OPTION VALUE=30>30
<OPTION VALUE=40>40
<OPTION VALUE=50>50
</SELECT>
</TD><TD>
&nbsp; |y(&#947;)|<  </TD> <TD>
  <SELECT NAME=etaa>
<OPTION VALUE=1  >1
<OPTION VALUE=1.5>1.5
<OPTION VALUE=2  >2
<OPTION VALUE=2.5 SELECTED>2.5
<OPTION VALUE=3  >3
<OPTION VALUE=4  >4
<OPTION VALUE=5  >5
<OPTION VALUE=1000>no cut
</SELECT>
</TD> <TD>
&nbsp; E(&#947;) > </TD> <TD>
<input type=text name=ea value=0 size=3> 
</TD></TR>
<TR> <TD>
leptons</TD><TD>
: p<SUB>T</SUB>(l) ></TD> <TD>
  <SELECT NAME=ptl>
<OPTION VALUE=0>no cut
<OPTION VALUE=10>10
<OPTION VALUE=20 SELECTED>20
<OPTION VALUE=30>30
<OPTION VALUE=40>40
<OPTION VALUE=50>50
</SELECT>
</TD><TD>
&nbsp; |y(l)|<  </TD> <TD>
  <SELECT NAME=etal>
<OPTION VALUE=1>1
<OPTION VALUE=1.5>1.5
<OPTION VALUE=2 >2
<OPTION VALUE=2.5 SELECTED>2.5
<OPTION VALUE=3>3
<OPTION VALUE=4>4
<OPTION VALUE=5>5
<OPTION VALUE=1000>no cut
</SELECT>
</TD> <TD>
&nbsp; E(l) > </TD> <TD>
<input type=text name=el value=0 size=3> 
</TD>
</TR>
</TABLE>

</P><P>
<font color="#0000FF"> Cuts on &#916;R<SUB>ij</SUB>:<BR>
<TABLE>
<TR> <TD>   </TD> <TD ALIGN=CENTER > j </TD> <TD ALIGN=CENTER > b </TD> <TD ALIGN=CENTER > &#947; </TD> <TD ALIGN=CENTER > l </TD>  </TR>
<TR> <TD> j </TD> <TD> <SELECT NAME=drjj>
  <OPTION VALUE=0>no cut
  <OPTION VALUE=0.3>0.3
  <OPTION VALUE=0.4 SELECTED>0.4
  <OPTION VALUE=0.5>0.5
  <OPTION VALUE=0.6>0.6
  <OPTION VALUE=0.7>0.7
  <OPTION VALUE=1>1
</SELECT>
  </TD> <TD> <SELECT NAME=drbj>
  <OPTION VALUE=0   SELECTED>no cut
  <OPTION VALUE=0.3>0.3
  <OPTION VALUE=0.4>0.4
  <OPTION VALUE=0.5>0.5
  <OPTION VALUE=0.6>0.6
  <OPTION VALUE=0.7>0.7
  <OPTION VALUE=1>1
</SELECT>
</TD> <TD>  <SELECT NAME=draj>
  <OPTION VALUE=0>no cut
  <OPTION VALUE=0.3>0.3
  <OPTION VALUE=0.4 SELECTED>0.4
  <OPTION VALUE=0.5>0.5
  <OPTION VALUE=0.6>0.6
  <OPTION VALUE=0.7>0.7
  <OPTION VALUE=1>1
</SELECT>
</TD> <TD> <SELECT NAME=drjl>
  <OPTION VALUE=0>no cut
  <OPTION VALUE=0.3>0.3
  <OPTION VALUE=0.4 SELECTED>0.4
  <OPTION VALUE=0.5>0.5
  <OPTION VALUE=0.6>0.6
  <OPTION VALUE=0.7>0.7
  <OPTION VALUE=1>1
</SELECT>
   </TD>  </TR>
<TR> <TD> b </TD> <TD></TD> <TD>
  <SELECT NAME=drbb>
  <OPTION VALUE=0   SELECTED>no cut
  <OPTION VALUE=0.3>0.3
  <OPTION VALUE=0.4>0.4
  <OPTION VALUE=0.5>0.5
  <OPTION VALUE=0.6>0.6
  <OPTION VALUE=0.7>0.7
  <OPTION VALUE=1>1
</SELECT>
</TD> <TD>
  <SELECT NAME=drab>
  <OPTION VALUE=0   SELECTED>no cut
  <OPTION VALUE=0.3>0.3
  <OPTION VALUE=0.4>0.4
  <OPTION VALUE=0.5>0.5
  <OPTION VALUE=0.6>0.6
  <OPTION VALUE=0.7>0.7
  <OPTION VALUE=1>1
</SELECT>
</TD> <TD>
  <SELECT NAME=drbl>
  <OPTION VALUE=0   SELECTED>no cut
  <OPTION VALUE=0.3>0.3
  <OPTION VALUE=0.4>0.4
  <OPTION VALUE=0.5>0.5
  <OPTION VALUE=0.6>0.6
  <OPTION VALUE=0.7>0.7
  <OPTION VALUE=1>1
</SELECT>
  </TD>  </TR>
<TR> <TD> &#947;
</TD><TD>
</TD><TD>
</TD> <TD>
  <SELECT NAME=draa>
    <OPTION VALUE=0>no cut
    <OPTION VALUE=0.3>0.3
    <OPTION VALUE=0.4 SELECTED>0.4
    <OPTION VALUE=0.5>0.5
    <OPTION VALUE=0.6>0.6
    <OPTION VALUE=0.7>0.7
    <OPTION VALUE=1>1
  </SELECT>
</TD> <TD>
  <SELECT NAME=dral>
    <OPTION VALUE=0>no cut
    <OPTION VALUE=0.3>0.3
    <OPTION VALUE=0.4 SELECTED>0.4
    <OPTION VALUE=0.5>0.5
    <OPTION VALUE=0.6>0.6
    <OPTION VALUE=0.7>0.7
    <OPTION VALUE=1>1
  </SELECT>
</TD>  </TR>
<TR> <TD> l
</TD> <TD>
</TD> <TD>
</TD> <TD>
</TD> <TD>
  <SELECT NAME=drll>
    <OPTION VALUE=0>no cut
    <OPTION VALUE=0.3>0.3
    <OPTION VALUE=0.4 SELECTED>0.4
    <OPTION VALUE=0.5>0.5
    <OPTION VALUE=0.6>0.6
    <OPTION VALUE=0.7>0.7
    <OPTION VALUE=1>1
  </SELECT>
</TD>  </TR>
</TABLE>
</P>

<P>
<font color="#0000FF"> Invariant mass cuts:<BR>
<TABLE>
<TR> <TD> jet-jet invariant mass </TD> <TD>: m<SUB>jj</SUB> > </TD> <TD>
  <SELECT NAME=mmjj>
<OPTION VALUE=0 SELECTED >no cut
<OPTION VALUE=10>10
<OPTION VALUE=20>20
<OPTION VALUE=30>30
<OPTION VALUE=40>40
<OPTION VALUE=50>50
<OPTION VALUE=60>60
<OPTION VALUE=70>70
<OPTION VALUE=80>80
<OPTION VALUE=90>90
<OPTION VALUE=100>100
</SELECT>
</TD></TR>
<TR> <TD> b-b  invariant mass </TD> <TD>: m<SUB>bb</SUB> >
</TD> <TD>
  <SELECT NAME=mmbb>
<OPTION VALUE=0 SELECTED >no cut
<OPTION VALUE=10>10
<OPTION VALUE=20>20
<OPTION VALUE=30>30
<OPTION VALUE=40>40
<OPTION VALUE=50>50
<OPTION VALUE=60>60
<OPTION VALUE=70>70
<OPTION VALUE=80>80
<OPTION VALUE=90>90
<OPTION VALUE=100>100
</SELECT>
</TD></TR>
<TR> <TD> &#947;-&#947; invariant mass </TD><TD>:  m<SUB>&#947;&#947;</SUB> >
</TD> <TD>
  <SELECT NAME=mmaa>
<OPTION VALUE=0 SELECTED >no cut
<OPTION VALUE=10>10
<OPTION VALUE=20>20
<OPTION VALUE=30>30
<OPTION VALUE=40>40
<OPTION VALUE=50>50
<OPTION VALUE=60>60
<OPTION VALUE=70>70
<OPTION VALUE=80>80
<OPTION VALUE=90>90
<OPTION VALUE=100>100
</SELECT>
</TD></TR>
 <TD> l<SUP>-</SUP>-l<SUP>+</SUP> invariant mass </TD><TD>:  m<SUB>ll</SUB> >
</TD> <TD>
  <SELECT NAME=mmll>
<OPTION VALUE=0 SELECTED >no cut
<OPTION VALUE=10>10
<OPTION VALUE=20>20
<OPTION VALUE=30>30
<OPTION VALUE=40>40
<OPTION VALUE=50>50
<OPTION VALUE=60>60
<OPTION VALUE=70>70
<OPTION VALUE=80>80
<OPTION VALUE=90>90
<OPTION VALUE=100>100
</SELECT>
</TD>
</TR>
</TABLE>


<P>
<font color="#0000FF"> Special cuts:<BR>
<TABLE>
<TR> <TD>
at least one jet with </TD><TD>
: p<SUB>T</SUB>(j) >
</TD> <TD>
  <SELECT NAME=xptj>
<OPTION VALUE=0 SELECTED >no cut
<OPTION VALUE=10>10
<OPTION VALUE=20>20
<OPTION VALUE=30>30
<OPTION VALUE=40>40
<OPTION VALUE=50>50
<OPTION VALUE=60>60
<OPTION VALUE=70>70
<OPTION VALUE=80>80
<OPTION VALUE=90>90
<OPTION VALUE=100>100
</SELECT>
</TD></TR>
<TR> <TD>
at least one b with </TD><TD>
: p<SUB>T</SUB>(b) >
</TD> <TD>
  <SELECT NAME=xptb>
<OPTION VALUE=0 SELECTED >no cut
<OPTION VALUE=10>10
<OPTION VALUE=20>20
<OPTION VALUE=30>30
<OPTION VALUE=40>40
<OPTION VALUE=50>50
<OPTION VALUE=60>60
<OPTION VALUE=70>70
<OPTION VALUE=80>80
<OPTION VALUE=90>90
<OPTION VALUE=100>100
</SELECT>
</TD></TR>
<TR> <TD>
at least one &#947; with </TD><TD>
: p<SUB>T</SUB>(&#947;) >
</TD> <TD>
  <SELECT NAME=xpta>
<OPTION VALUE=0 SELECTED >no cut
<OPTION VALUE=10>10
<OPTION VALUE=20>20
<OPTION VALUE=30>30
<OPTION VALUE=40>40
<OPTION VALUE=50>50
<OPTION VALUE=60>60
<OPTION VALUE=70>70
<OPTION VALUE=80>80
<OPTION VALUE=90>90
<OPTION VALUE=100>100
</SELECT>
</TD></TR>
<TR> <TD>
at least one lepton with </TD><TD>
: p<SUB>T</SUB>(l) >
</TD> <TD>
  <SELECT NAME=xptl>
<OPTION VALUE=0 SELECTED >no cut
<OPTION VALUE=10>10
<OPTION VALUE=20>20
<OPTION VALUE=30>30
<OPTION VALUE=40>40
<OPTION VALUE=50>50
<OPTION VALUE=60>60
<OPTION VALUE=70>70
<OPTION VALUE=80>80
<OPTION VALUE=90>90
<OPTION VALUE=100>100
</SELECT>
</TD>

</TR>
</TABLE>
<br>
<font color="#0000FF"> WBF cuts: 
the two hardest jets y(j1)*y(j2)<0 and: <br>
|y(j1)|, |y(j2)| > y<SUB>min</SUB> and/or &#916y(j1,j2)> &#916y<SUB>min</SUB> <br>
<font color="#000000">
y<SUB>min</SUB> >  <SELECT NAME=xetamin>
<OPTION VALUE=0 SELECTED >no cut
<OPTION VALUE=1>1
<OPTION VALUE=1.5>1.5
<OPTION VALUE=2>2
<OPTION VALUE=2.5>2.5
<OPTION VALUE=3>3
</SELECT>
<br>
&#916y<SUB>min</SUB> >  <SELECT NAME=deltaeta>
<OPTION VALUE=0 SELECTED >no cut
<OPTION VALUE=1>1
<OPTION VALUE=1.5>1.5
<OPTION VALUE=2>2
<OPTION VALUE=2.5>2.5
<OPTION VALUE=3>3
</SELECT>

<br><br>
</td>
</tr>

<tr>
<td valign="top">
<br>
<center><font color="#008080"><font size=+2><U><a name=plot_form>Plotting Card</a></U></font></font> </center>
<br>
<table border=1 width=90% align=center>
<tr><td>
<table>
<tr><td>
Choose an option for the plot_card.dat:
 <select name=plot_card>
 <option value=present> Use the present file
 <option value=upload> Upload a file
 <option value=default> Use the default file
 <option value=norun> Do not plot
 </select>
</td></tr>
<tr><td>
The present <a href="http://$server/$dirHTML/$process/Cards/plot_card.dat">plot_card.dat</a>
<INPUT TYPE=SUBMIT NAME="Edit" VALUE="Edit plot_card.dat">
</td></tr>
<tr><td>
The <a href="http://$server/$dirHTML/$process/Cards/plot_card_default.dat">default file</a>
</td></tr>
<tr><td>
Upload a plot_card: <INPUT TYPE="FILE" NAME="plot_card.dat">  </td>
</td></tr>
</table>
</td></tr>
</table>
<BR>
ENDPRINT

&PrintPythiaForm;
&PrintPGSForm;

    print <<ENDPRINT;
</td>


</table>  
</FORM> 
</body>
</html>
ENDPRINT
}

sub PrintPythiaForm()
{
    print <<ENDPRINT;

<!--   PYTHIA PART    -->
<tr>
<td valign="top">
<br>
<center><font color="#008080"><font size=+2><U><a name=pythia_form>Pythia - showering and hadronization</a></U></font></font> </center>
<br>
<table border=1 width=90% align=center>
<tr><td>
<table>
<tr><td>
Choose an option for the pythia_card.dat:
 <select name=pythia_card>
ENDPRINT
if($whattodo ne "pythia" && $card ne "pythia_card.dat" && $card ne "pgs_card.dat" && $card ne "delphes_card.dat"){
    print "<option value=norun> Do not run Pythia\n";
}
if ( -e "$directory/Cards/pythia_card.dat"){
    print "<option value=present selected> Use the present file\n";
}
print <<ENDPRINT;
 <option value=form> Fill in the form below
 <option value=upload> Upload a file
 <option value=default> Use the default file
 </select>
</td></tr>
<tr><td>
ENDPRINT
if ( -e "$directory/Cards/pythia_card.dat"){
    print "The present <a href=\"http://$server/$dirHTML/$process/Cards/pythia_card.dat\">pythia_card.dat</a>\n";
} else {
    print "No present pythia_card.dat\n";
}
print "<INPUT TYPE=SUBMIT NAME=\"Edit\" VALUE=\"Edit pythia_card.dat\">\n";
print <<ENDPRINT;
</td></tr>
<tr><td>
The <a href="http://$server/$dirHTML/$process/Cards/pythia_card_default.dat">default file</a>
</td></tr>
<tr><td>
Upload a pythia_card: <INPUT TYPE="FILE" NAME="pythia_card.dat">  </td>
</td></tr>
</table>
</td></tr>
</table>
<BR>
<table>
  <tr>
    <td>Shower type</td>
    <td>
      <select name=shower>
	<option value=20 selected> New (pT)
	<option value=0 > Old (Q2)
      </select>
    </td>
  </tr>

    <td>Showering</td>
    <td>
      <select name=showering>
	<option value=1 selected> ON
	<option value=0 > OFF
      </select>
    </td>
  </tr>

  <tr>
    <td>Hadronization</td>
    <td>
      <select name=hadronization>
 	<option value=1 selected> ON
	<option value=0 > OFF
      </select>
    </td>
  </tr>

  <tr>
    <td>Multiple interactions</td>
    <td>
      <select name=minteractions>
 	<option value=1 > ON
	<option value=0 selected> OFF
      </select>
    </td>
  </tr>

</table>
<br>
<!--<table>
  <tr>
    <td>Print Jet Rates?</td>
    <td>
      <select name=djrchoice>
        <option value=no selected> No
        <option value=yes> Yes
      </select>
    </td>
  </tr>
</table>
-->
</td>
</tr>
ENDPRINT
}

sub PrintPGSForm()
{
    print <<ENDPRINT;
<!--   PGS PART    -->
<tr>
<td valign="top">
<br>
<center><font color="#008080"><font size=+2><U><a name=pgs_form>Detector simulation</a></U></font></font> </center>
<br>
<table border=1 width=90% align=center>
<tr><td>
<table>
<tr><td>
Choose detector simulation software:
 <select name=det_sim>
ENDPRINT
if($whattodo ne "pgs"){
    print "<option value=norun> Do not run detector simulation\n";
}
if ($card eq "pgs_card.dat" || -e "$directory/Cards/pgs_card.dat" || -e "$directory/Cards/delphes_card.dat"){
    print "<option value=pgs selected> PGS\n";
}else{
    print "<option value=pgs> PGS\n";
}
    $delphesexists=( -e "$directory/Cards/delphes_card_default.dat");
if($card eq "delphes_card.dat" || $card eq "delphes_trigger.dat" || ( -e "$directory/Cards/delphes_card.dat" && ! -e "$directory/Cards/pgs_card.dat")){
    print "<option value=delphes selected> Delphes\n";
}elsif ($delphesexists){
    print "<option value=delphes> Delphes\n";
}
print <<ENDPRINT;
 </select>
</td></tr>
<tr><td>
Choose an option for the pgs_card.dat:
 <select name=det_options>
ENDPRINT
if ( -e "$directory/Cards/pgs_card.dat" || -e "$directory/Cards/delphes_card.dat"){
    print "<option value=present selected> Use the present file\n";
}
print <<ENDPRINT;
 <option value=form> Fill in the form below
 <option value=upload> Upload a file
 <option value=default> Use the default file
 </select>
</td></tr>
<tr><td>
ENDPRINT
if ( -e "$directory/Cards/pgs_card.dat"){
    print "The present <a href=\"http://$server/$dirHTML/$process/Cards/pgs_card.dat\">pgs_card.dat</a>\n";
#} else {
#    print "No present pgs_card.dat\n";
}
print "<INPUT TYPE=SUBMIT NAME=\"Edit\" VALUE=\"Edit pgs_card.dat\">";
if ( -e "$directory/Cards/delphes_card.dat"){
    print "The present <a href=\"http://$server/$dirHTML/$process/Cards/delphes_card.dat\">delphes_card.dat</a>\n";
    if ( -e "$directory/Cards/delphes_trigger.dat"){
	print " <a href=\"http://$server/$dirHTML/$process/Cards/delphes_trigger.dat\">delphes_trigger.dat</a>\n";
    }
}
if($delphesexists){
    print " | <INPUT TYPE=SUBMIT NAME=\"Edit\" VALUE=\"Edit delphes_card.dat\">\n";
    print "<INPUT TYPE=SUBMIT NAME=\"Edit\" VALUE=\"Edit delphes_trigger.dat\">\n";
}
print <<ENDPRINT;
</td></tr>
<tr><td>
The <a href="http://$server/$dirHTML/$process/Cards/pgs_card_default.dat">default PGS file</a>
ENDPRINT
if($delphesexists){
    print" | <a href=\"http://$server/$dirHTML/$process/Cards/delphes_card_default.dat\">default Delphes file</a>\n";
}
print <<ENDPRINT;
</td></tr>
<tr><td>
Upload a pgs_card: <INPUT TYPE="FILE" NAME="pgs_card.dat">
ENDPRINT
if($delphesexists){
    print "<br>\nUpload a delphes_card: <INPUT TYPE=\"FILE\" NAME=\"delphes_card.dat\"> delphes_trigger: <INPUT TYPE=\"FILE\" NAME=\"delphes_trigger.dat\">\n";
}
print <<ENDPRINT;
</td></tr>
</table>
</td></tr>
</table>
<BR>
<table>
  <tr>
    <td>Detector type</td>
    <td>
      <select name=experiment>
	<option value=lhc selected> LHC
	<option value=atlas> ATLAS
	<option value=cms> CMS
	<option value=tev> Tevatron
      </select>
    </td>
  </tr>
</table>
ENDPRINT
}

sub HandlePythiaForm(){
    $pythia_card=$in{"pythia_card"};
    
    if($pythia_card eq "form"){
	$shower=$in{"shower"};
	$showering=$in{"showering"};
	$hadronization=$in{"hadronization"};
	$minteractions=$in{"minteractions"};
	&WritePythiaCard($dirMAD);
    }
    elsif($pythia_card eq "upload") {
	if($files{"pythia_card.dat"} eq ""){
	    WriteError("No valid pythia_card file supplied. Please choose a pythia_card for upload.");
	}
    }	
    elsif($pythia_card eq "default") {
	if(!(-e "$directory/Cards/pythia_card_default.dat")){
	    WriteError("The file pythia_card_default.dat was not found. Please choose another generation method.");
	}
	system("cp $directory/Cards/pythia_card_default.dat $directory/Cards/pythia_card.dat");
    }
    elsif($pythia_card eq "norun") {
	system("rm -f $directory/Cards/pythia_card.dat >& /dev/null");
    }
}

sub HandleDetectorForm(){
    $det_sim=$in{"det_sim"};
    $det_options=$in{"det_options"};

    if(($det_sim eq "delphes") && (! -e "$directory/Cards/delphes_card_default.dat")){
	WriteError("Sorry, Delphes not available. Please use PGS instead.");
    }

    if($det_sim eq "pgs"){
	if($det_options eq "form"){
	    $experiment=$in{"experiment"};
	    
	    if($experiment eq 'lhc'){
		system ("cp  $directory/Cards/pgs_card_LHC.dat $directory/Cards/pgs_card.dat")}
	    if($experiment eq 'atlas'){
		system ("cp  $directory/Cards/pgs_card_ATLAS.dat $directory/Cards/pgs_card.dat")}
	    elsif($experiment eq 'cms'){
		system ("cp  $directory/Cards/pgs_card_CMS.dat $directory/Cards/pgs_card.dat")}
	    elsif($experiment eq 'tev'){
		system ("cp  $directory/Cards/pgs_card_TEV.dat $directory/Cards/pgs_card.dat")}
	}
	elsif($det_options eq "upload") {
	    if($files{"pgs_card.dat"} eq ""){
		WriteError("No valid pgs_card file supplied. Please choose a file for upload.");
	    }
	}	
	elsif($det_options eq "default") {
	    if(!(-e "$directory/Cards/pgs_card_default.dat")){
		WriteError("The file pgs_card_default.dat was not found. Please choose another generation method.");
	    }
	    system("cp $directory/Cards/pgs_card_default.dat $directory/Cards/pgs_card.dat");
	}
	if($det_options eq "present"){
	    if(!-e "$directory/Cards/pgs_card.dat" && $files{"pgs_card.dat"} eq ""){
		WriteError("No valid pgs_card file. Please generate a file.");
	    }
	}
	system("rm $directory/Cards/delphes_card.dat >& /dev/null");
	system("rm $directory/Cards/delphes_trigger.dat >& /dev/null");
    }
    elsif ($det_sim eq "delphes"){
	if($det_options eq "form"){
	    $experiment=$in{"experiment"};
	    
	    if($experiment eq 'lhc'){
		system ("cp  $directory/Cards/delphes_card_CMS.dat $directory/Cards/delphes_card.dat");
		system ("cp  $directory/Cards/delphes_trigger_CMS.dat $directory/Cards/delphes_trigger.dat")}
	    if($experiment eq 'atlas'){
		system ("cp  $directory/Cards/delphes_card_ATLAS.dat $directory/Cards/delphes_card.dat");
		system ("cp  $directory/Cards/delphes_trigger_ATLAS.dat $directory/Cards/delphes_trigger.dat")}
	    elsif($experiment eq 'cms'){
		system ("cp  $directory/Cards/delphes_card_CMS.dat $directory/Cards/delphes_card.dat");
		system ("cp  $directory/Cards/delphes_trigger_CMS.dat $directory/Cards/delphes_trigger.dat")}
	    elsif($experiment eq 'tev'){
		if(!(-e "$directory/Cards/delphes_card_TEV.dat")){
		    WriteError("Sorry, no Tevatron Delphes card available. Please choose PGS or another detector.");
		} else {
		    system ("cp  $directory/Cards/delphes_card_TEV.dat $directory/Cards/delphes_card.dat");
		    system ("cp  $directory/Cards/delphes_trigger_TEV.dat $directory/Cards/delphes_trigger.dat");
		}
	    }
	}
	elsif($det_options eq "upload") {
	    if($files{"delphes_card.dat"} eq "" && $files{"delphes_trigger.dat"} eq ""){
		WriteError("No valid delphes_card or delphes_trigger file supplied. Please choose a file for upload.");
	    }
	    if(!-e "$directory/Cards/delphes_card.dat" && $files{"delphes_card.dat"} eq ""){
		WriteError("No valid delphes_card file supplied. Please choose a file for upload.");
	    }
	    if(!-e "$directory/Cards/delphes_trigger.dat" && $files{"delphes_trigger.dat"} eq ""){
		WriteError("No valid delphes_trigger file supplied. Please choose a file for upload.");
	    }
	}	
	elsif($det_options eq "default") {
	    if(!(-e "$directory/Cards/delphes_card_default.dat")){
		WriteError("The file delphes_card_default.dat was not found. Please choose another generation method.");
	    }
	    system("cp $directory/Cards/delphes_card_default.dat $directory/Cards/delphes_card.dat");
	    system("cp $directory/Cards/delphes_trigger_default.dat $directory/Cards/delphes_trigger.dat");
	}
	if($det_options eq "present"){
	    if(!-e "$directory/Cards/delphes_card.dat" && $files{"delphes_card.dat"} eq ""){
		WriteError("No valid delphes_card file. Please generate a file.");
	    }
	    if(!-e "$directory/Cards/delphes_trigger.dat" && $files{"delphes_trigger.dat"} eq ""){
		WriteError("No valid delphes_trigger file. Please generate a file.");
	    }
	}
	system("rm -f $directory/Cards/pgs_card.dat >& /dev/null");
    }
    else {
	system("rm -f $directory/Cards/pgs_card.dat $directory/Cards/delphes_card.dat $directory/Cards/delphes_trigger.dat >& /dev/null");	
    }
}

1
