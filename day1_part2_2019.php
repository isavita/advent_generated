
<?php

function processLine($line) {
    $n = intval(trim($line));
    return $n;
}

function getTotal($masses) {
    $total = 0;

    foreach ($masses as $mass) {
        $total += calcFuelMass($mass);
    }

    return $total;
}

function calcFuelMass($mass) {
    $fuel = (floor($mass / 3) - 2);
    if ($fuel <= 0) {
        return 0;
    }

    return $fuel + calcFuelMass($fuel);
}

$file = fopen("input.txt", "r");
$masses = [];

while (!feof($file)) {
    $line = trim(fgets($file));
    $n = processLine($line);
    $masses[] = $n;
}

$total = getTotal($masses);

echo $total . PHP_EOL;

fclose($file);
?>
