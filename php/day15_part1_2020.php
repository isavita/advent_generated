
<?php

$input = file_get_contents("input.txt");
$startingNumbers = explode(",", trim($input));

$lastSpoken = [];
$lastNumber = 0;
$nextNumber = 0;

for ($turn = 1; $turn <= 2020; $turn++) {
    if ($turn - 1 < count($startingNumbers)) {
        $lastNumber = (int)$startingNumbers[$turn - 1];
        $lastSpoken[$lastNumber] = $turn;
        continue;
    }
    if (array_key_exists($lastNumber, $lastSpoken) && $lastSpoken[$lastNumber] != $turn - 1) {
        $nextNumber = $turn - 1 - $lastSpoken[$lastNumber];
    } else {
        $nextNumber = 0;
    }
    $lastSpoken[$lastNumber] = $turn - 1;
    $lastNumber = $nextNumber;
}

echo $lastNumber . "\n";
?>
