
<?php

$input = file_get_contents("input.txt");
$parts = explode("-", $input);
$start = intval($parts[0]);
$end = intval($parts[1]);

$count = 0;
for ($i = $start; $i <= $end; $i++) {
    $s = strval($i);
    if (hasDoubleAndIncreasingDigits($s)) {
        $count++;
    }
}

echo $count . PHP_EOL;

function hasDoubleAndIncreasingDigits($s) {
    $hasDouble = false;
    for ($i = 0; $i < strlen($s) - 1; $i++) {
        if ($s[$i] == $s[$i + 1]) {
            $hasDouble = true;
        }
        if ($s[$i] > $s[$i + 1]) {
            return false;
        }
    }
    return $hasDouble;
}
?>
