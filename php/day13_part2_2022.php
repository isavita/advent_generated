
<?php

$input = file_get_contents("input.txt");
$packets = [];
foreach (explode("\n\n", $input) as $pair) {
    $sp = explode("\n", $pair);
    $first = json_decode($sp[0]);
    $second = json_decode($sp[1]);
    $packets[] = $first;
    $packets[] = $second;
}

$divider1 = json_decode("[[2]]");
$divider2 = json_decode("[[6]]");
$packets[] = $divider1;
$packets[] = $divider2;
usort($packets, function($a, $b) {
    return compare($a, $b);
});
$divider1Pos = array_search($divider1, $packets);
$divider2Pos = array_search($divider2, $packets);
echo (($divider1Pos + 1) * ($divider2Pos + 1)) . "\n";

function compare($a, $b) {
    if (is_numeric($a) && is_numeric($b)) {
        return sign((int)$a - (int)$b);
    } elseif (is_numeric($a)) {
        return compare([$a], $b);
    } elseif (is_numeric($b)) {
        return compare($a, [$b]);
    } else {
        $aa = (array)$a;
        $bb = (array)$b;
        $len = min(count($aa), count($bb));
        for ($i = 0; $i < $len; $i++) {
            $c = compare($aa[$i], $bb[$i]);
            if ($c != 0) {
                return $c;
            }
        }
        return sign(count($aa) - count($bb));
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
