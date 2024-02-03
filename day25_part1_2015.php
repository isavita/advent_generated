
<?php

$data = file_get_contents("input.txt");

preg_match('/row (\d+), column (\d+)/', $data, $matches);
if (count($matches) != 3) {
    die("Invalid input format.");
}

$row = intval($matches[1]);
$column = intval($matches[2]);

$pos = ($row + $column - 2) * ($row + $column - 1) / 2 + $column;
$code = getCode($pos);

echo $code . "\n";

function getCode($position) {
    $startCode = 20151125;
    $multiplier = 252533;
    $modulus = 33554393;

    $code = $startCode;
    for ($i = 1; $i < $position; $i++) {
        $code = ($code * $multiplier) % $modulus;
    }
    return $code;
}
?>
