#!/usr/bin/perl -w

#############################################################################
#                                                                          ##
#                    MadGraph/MadEvent                                     ##
#                                                                          ##
# FILE : register_page.pl                                                  ##
# VERSION : 1.0                                                            ##
# DATE : 10 April 2007                                                     ##
# AUTHOR : Michel Herquet (UCL-CP3)                                        ##
#                                                                          ##
# DESCRIPTION : CGI script to display the register page with a captcha     ##
#############################################################################
# Import the necessary module.
use WebService::CaptchasDotNet;

# Construct the captchas object. Replace 'demo' and 'secret' with
# the values you receive upon registration at http://captchas.net.
my $captchas = WebService::CaptchasDotNet->new(secret   => 'lRzV48CeZVw6ggwXq3tFZEUrm3DBw0IuIPo7N5I3',
                                               username => 'madgraph');


my $random = $captchas->random ();
my $url = $captchas->url ($random);

print "Content-type: text/html\n\n";
print "<html>
<head>
<title>VBF @ NNLO Registration</title>
<META HTTP-EQUIV=\"EXPIRES\" CONTENT=\"20\" >
</head>
<center><h1> VBF @ NNLO Registration </h1>
<h3> Please complete the form below. 
Your username and password will be sent 
to the e-mail address you enter. </h3></center>


<FORM METHOD=\"post\" ACTION=\"/cgi-bin/Register/register_vbf-pl
\" NAME= \"Register\">


First Name                    <INPUT TYPE=\"text\" NAME=\"first_name\" MAXLENGTH=\"255\"><br><br>
Family Name                   <INPUT TYPE=\"text\" NAME=\"last_name\" MAXLENGTH=\"255\"><br><br>
Name of your institution      <INPUT TYPE=\"text\" NAME=\"inst\" MAXLENGTH=\"255\"><br><br>
Your e-mail address           <INPUT TYPE=\"text\" NAME=\"email\" MAXLENGTH=\"255\"><br><br>
The letter sequence you can read on the following image:        <input name=\"password\" size=\"6\" />
<br>
<a href=\"http://captchas.net\">
<img style=\"border: none; vertical-align: bottom\" 
src=\"$url\" alt=\"The CAPTCHA image\" />
</a>
<input type=\"hidden\" name=\"random\" value=\"$random\" />
<INPUT TYPE=\"submit\" VALUE=\"SUBMIT\">
</FORM>
<br>
";

