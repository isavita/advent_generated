
<?php

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);
$freq = 0;

foreach ($lines as $change) {
    $freq += parseChange($change);
}

echo $freq . PHP_EOL;

function parseChange($change) {
    list($sign, $num) = parseSignNum($change);
    return $sign * $num;
}

function parseSignNum($change) {
    $sign = 1;
    if ($change[0] == '-') {
        $sign = -1;
        $change = substr($change, 1);
    }
    $num = intval($change);
    if ($num === 0 && $change !== "0") {
        throw new Exception("invalid frequency change: $change");
    }
    return array($sign, $num);
}
?>
