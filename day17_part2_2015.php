
<?php

$containers = file("input.txt", FILE_IGNORE_NEW_LINES);

function findCombinations($containers, $target, $index, $count, &$minCount, &$ways) {
    if ($target == 0) {
        if ($minCount == 0 || $count < $minCount) {
            $minCount = $count;
            $ways = 1;
        } elseif ($count == $minCount) {
            $ways++;
        }
        return;
    }
    if ($target < 0 || $index >= count($containers)) {
        return;
    }
    // Include current container
    findCombinations($containers, $target - $containers[$index], $index + 1, $count + 1, $minCount, $ways);
    // Exclude current container
    findCombinations($containers, $target, $index + 1, $count, $minCount, $ways);
}

$minCount = 0;
$ways = 0;
findCombinations($containers, 150, 0, 0, $minCount, $ways);
echo $ways . PHP_EOL;
?>
