
<?php

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);
$adapters = array_map('intval', $lines);
sort($adapters);

$joltDifferences = [3 => 1];
$previousJoltage = 0;

foreach ($adapters as $adapter) {
    $diff = $adapter - $previousJoltage;
    $joltDifferences[$diff]++;
    $previousJoltage = $adapter;
}

$product = $joltDifferences[1] * $joltDifferences[3];
echo $product . "\n";
?>
