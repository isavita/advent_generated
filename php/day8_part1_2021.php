<?php
$input = file_get_contents("input.txt");
$lines = explode("\n", $input);
$count = 0;
foreach ($lines as $line) {
    $parts = explode("|", $line);
    $outputs = explode(" ", trim($parts[1]));
    foreach ($outputs as $output) {
        $length = strlen($output);
        if ($length == 2 || $length == 4 || $length == 3 || $length == 7) {
            $count++;
        }
    }
}
echo $count;
?>