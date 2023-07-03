<?php
# Directory Index (dirindex.php)

# Reads the current directory's content and displays it as
# HTML.  Useful if file listing is denied by the web server
# configuration.

# Installation:
# - Put in any directory you like on your PHP-capable webspace.
# - Rename to 'index.php' if you like it to get called if no
#   file is specified in the URL (e.g. www.example.com/files/).
# - Fit the design to your needs just using HTML and CSS.

# Version: 25-Mar-2002
# Copyright (c) 2002 Jochen Kupperschmidt
# Released under the terms of the GNU General Public License

# Configuration
$show_path = 1;   # Show local path.
$show_dotdirs = 1;   # Show '.' and '..'.

$path = substr($_SERVER['SCRIPT_FILENAME'], 0,
    strrpos($_SERVER['SCRIPT_FILENAME'], '/') + 1);
?>
<html>
  <head>
    <title>Directory Index</title>
    <style type="text/css">
      body {
        font-family: Courier, Verdana, Arial, sans-serif;
        margin: 80px;
        text-align: center;
      }

      body,
      th,
      td {
        background-color: #ffffff;
      }

      a:link {
        color: #666666;
        text-decoration: underline;
      }
      a:visited {
        color: #444444;
        text-decoration: underline;
      }
      a:hover {
        color: #666666;
        text-decoration: none;
      }
      a:active {
        color: #660000;
        text-decoration: none;
      }

      table {
        background-color: #ffffff;
        
        border-spacing: 1px;
        width: 680px;
      }
      th {
        background-color: #ffffff;
        color: #ffffff;
        font-size: 12pt;
        font-weight: bold;
        text-align: left;
        padding: 2px;
      }
      td {
        background-color: #ffffff;
        color: #666666;
        font-size: 12pt;
        font-weight: normal;
        padding: 6px;
      }
    </style>
  </head>
  <body>

    <table cellspacing="1">
      <tr>
        <th><?php if ($show_path == 1) { echo $path; } else { echo 'content of this directory'; } ?></th>
      </tr>
      <tr>
        <td>
<?php
$dirs = array();
$files = array();

$dir = dir($path);
while ($entry = $dir->read()) {
    if (($entry != '.') and (substr($entry, -4) != '.php')) {
        if (is_dir($entry)) {
            if (($entry != '..') or $show_dotdirs){
                $dirs[] = $entry;
            }
        } else {
            $files[] = $entry;
        }
    }
}
$dir->close();

sort($dirs);
foreach ($dirs as $dir) {
    printf('<strong>&lt;</strong> <a href="%s">%s</a> <strong>&gt;</strong><br />' . "\n", $dir, $dir);
}

sort($files);
foreach ($files as $file) {
    printf('<a href="%s">%s</a><br />' . "\n", $file, $file);
}
?>
        </td>
      </tr>
    </table>


  </body>
</html>
