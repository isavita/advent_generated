
<?php

$input = file_get_contents("input.txt");

$totalCups = 1000000;
$totalMoves = 10000000;

$cups = array_fill(0, $totalCups + 1, 0);
$lastCup = 0;

for ($i = 0; $i < strlen($input); $i++) {
    $cup = (int)$input[$i];
    if ($i > 0) {
        $cups[$lastCup] = $cup;
    }
    $lastCup = $cup;
}

for ($i = strlen($input) + 1; $i <= $totalCups; $i++) {
    $cups[$lastCup] = $i;
    $lastCup = $i;
}
$cups[$lastCup] = (int)$input[0];

$currentCup = (int)$input[0];
for ($i = 0; $i < $totalMoves; $i++) {
    $pickup1 = $cups[$currentCup];
    $pickup2 = $cups[$pickup1];
    $pickup3 = $cups[$pickup2];

    $cups[$currentCup] = $cups[$pickup3];

    $destinationCup = $currentCup - 1;
    if ($destinationCup == 0) {
        $destinationCup = $totalCups;
    }
    while ($destinationCup == $pickup1 || $destinationCup == $pickup2 || $destinationCup == $pickup3) {
        $destinationCup--;
        if ($destinationCup == 0) {
            $destinationCup = $totalCups;
        }
    }

    $cups[$pickup3] = $cups[$destinationCup];
    $cups[$destinationCup] = $pickup1;

    $currentCup = $cups[$currentCup];
}

$cup1 = $cups[1];
$cup2 = $cups[$cup1];
echo $cup1 * $cup2 . PHP_EOL;
?>
