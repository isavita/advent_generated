
<?php

$handle = fopen("input.txt", "r");
$count = 0;

while (($line = fgets($handle)) !== false) {
    $pair = explode(",", $line);

    $left = parseRange($pair[0]);
    $right = parseRange($pair[1]);

    if ($left[0] <= $right[1] && $left[1] >= $right[0]) {
        $count++;
    }
}

fclose($handle);

echo $count . PHP_EOL;

function parseRange($s) {
    $split = explode("-", $s);
    $start = (int)$split[0];
    $end = (int)$split[1];
    return [$start, $end];
}
?>
