
<?php

$input = file_get_contents("input.txt");
$steps = (int)trim($input);
$currentPos = 0;
$valueAfterZero = 0;

for ($i = 1; $i <= 50000000; $i++) {
    $currentPos = ($currentPos + $steps) % $i;
    if ($currentPos == 0) {
        $valueAfterZero = $i;
    }
    $currentPos++;
}

echo $valueAfterZero . "\n";
?>
