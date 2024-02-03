
<?php

$containers = file("input.txt", FILE_IGNORE_NEW_LINES);

function countCombinations($containers, $target, $index) {
    if ($target == 0) {
        return 1;
    }
    if ($target < 0 || $index >= count($containers)) {
        return 0;
    }
    return countCombinations($containers, $target - $containers[$index], $index + 1) +
           countCombinations($containers, $target, $index + 1);
}

echo countCombinations($containers, 150, 0) . PHP_EOL;

?>
