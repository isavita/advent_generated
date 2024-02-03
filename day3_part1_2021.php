
<?php

$file = fopen("input.txt", "r");
if ($file === false) {
    die("Unable to open file");
}

$counts = array_fill(0, 12, array(0, 0));

while (($num = fgets($file)) !== false) {
    $num = trim($num);
    for ($i = 0; $i < strlen($num); $i++) {
        $counts[$i][$num[$i]]++;
    }
}

fclose($file);

$gammaRate = 0;
$epsilonRate = 0;

for ($i = 0; $i < count($counts); $i++) {
    if ($counts[$i][0] > $counts[$i][1]) {
        $gammaRate |= 1 << (count($counts) - $i - 1);
    } else {
        $epsilonRate |= 1 << (count($counts) - $i - 1);
    }
}

echo $gammaRate * $epsilonRate . PHP_EOL;
?>
