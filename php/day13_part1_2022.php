
<?php

$input = file_get_contents("input.txt");
$packets = [];
$sum = 0;
$pairs = explode("\n\n", $input);
foreach ($pairs as $i => $pair) {
    $sp = explode("\n", $pair);
    $first = json_decode($sp[0]);
    $second = json_decode($sp[1]);
    $packets[] = $first;
    $packets[] = $second;
    if (compare($first, $second) == -1) {
        $sum += $i + 1;
    }
}
echo $sum . PHP_EOL;

function compare($a, $b) {
    if (is_numeric($a) && is_numeric($b)) {
        return sign((int)$a - (int)$b);
    } elseif (is_numeric($a)) {
        return compare([$a], $b);
    } elseif (is_numeric($b)) {
        return compare($a, [$b]);
    } else {
        $lenA = count($a);
        $lenB = count($b);
        for ($i = 0; $i < $lenA && $i < $lenB; $i++) {
            $c = compare($a[$i], $b[$i]);
            if ($c != 0) {
                return $c;
            }
        }
        return sign($lenA - $lenB);
    }
}

function sign($n) {
    if ($n == 0) {
        return 0;
    }
    if ($n < 0) {
        return -1;
    }
    return 1;
}
?>
