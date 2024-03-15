<?php
$file = fopen("input.txt", "r");
$adapters = [0];

while (($line = fgets($file)) !== false) {
    $joltage = intval(trim($line));
    $adapters[] = $joltage;
}
fclose($file);

sort($adapters);
$adapters[] = $adapters[count($adapters) - 1] + 3;

echo countArrangements($adapters) . "\n";

function countArrangements($adapters) {
    $ways = array_fill(0, max($adapters) + 1, 0);
    $ways[0] = 1;

    for ($i = 1; $i < count($adapters); $i++) {
        $currentJoltage = $adapters[$i];
        for ($diff = 1; $diff <= 3; $diff++) {
            $ways[$currentJoltage] += $ways[$currentJoltage - $diff];
        }
    }

    return $ways[$adapters[count($adapters) - 1]];
}
?>