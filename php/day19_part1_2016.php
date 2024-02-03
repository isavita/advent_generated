
<?php

$filename = "input.txt";
$totalElves = (int)file_get_contents($filename);
$winner = findWinningElf($totalElves);
echo $winner . PHP_EOL;

function findWinningElf($totalElves) {
    $highestPowerOfTwo = 1;
    while ($highestPowerOfTwo * 2 <= $totalElves) {
        $highestPowerOfTwo *= 2;
    }
    return ($totalElves - $highestPowerOfTwo) * 2 + 1;
}
?>
