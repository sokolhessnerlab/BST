<?php
// the $_POST[] array will contain the passed in filename and data
// the directory "data" is writable by the server (chmod 777)
$datetimetxt = date('Y-m-d-H-i');
$filename = "data/".$_POST['filename']."-".$datetimetxt.".csv";
$data = $_POST['filedata'];

// write the file to disk
file_put_contents($filename, $data);
?>
