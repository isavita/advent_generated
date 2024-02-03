
<?php

$masses = [];
$total = 0;

$handle = fopen("input.txt", "r");
if ($handle) {
    while (($line = fgets($handle)) !== false) {
        $mass = intval(trim($line));
        $masses[] = $mass;
    }

    fclose($handle);
} else {
    echo "Error opening the file";
}

foreach ($masses as $m) {
    $total += (floor($m / 3) - 2);
}

echo $total . PHP_EOL;
?>
