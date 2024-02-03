
<?php

$numbers = file("input.txt", FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);

$invalidNumber = 14360655;

for ($i = 0; $i < count($numbers); $i++) {
    $sum = $numbers[$i];
    $min = $numbers[$i];
    $max = $numbers[$i];
    for ($j = $i + 1; $j < count($numbers); $j++) {
        $sum += $numbers[$j];
        if ($numbers[$j] < $min) {
            $min = $numbers[$j];
        }
        if ($numbers[$j] > $max) {
            $max = $numbers[$j];
        }
        if ($sum == $invalidNumber) {
            echo $min + $max . "\n";
            return;
        } elseif ($sum > $invalidNumber) {
            break;
        }
    }
}
?>
