<?php

const SIZE = 119315717514047;

function modinv($a, $m) {
    $m0 = $m;
    $x0 = 0;
    $x1 = 1;
    if ($m == 1) return 0;
    while ($a > 1) {
        $q = intdiv($a, $m);
        $t = $m;
        $m = $a % $m;
        $a = $t;
        $t = $x0;
        $x0 = $x1 - $q * $x0;
        $x1 = $t;
    }
    if ($x1 < 0) $x1 += $m0;
    return $x1;
}

$size = gmp_init(SIZE);
$iter = gmp_init('101741582076661');
$offset = gmp_init(0);
$increment = gmp_init(1);

$file = fopen("input.txt", "r");
if (!$file) {
    throw new Exception("Unable to open file!");
}

while (($line = fgets($file)) !== false) {
    $line = trim($line);
    if ($line === "deal into new stack") {
        $increment = gmp_mul($increment, -1);
        $offset = gmp_add($offset, $increment);
    } elseif (strpos($line, "cut") === 0) {
        $parts = explode(' ', $line);
        $n = intval($parts[1]);
        $offset = gmp_add($offset, gmp_mul($n, $increment));
    } elseif (strpos($line, "deal with increment") === 0) {
        $parts = explode(' ', $line);
        $n = intval(end($parts));
        $increment = gmp_mul($increment, gmp_powm($n, gmp_sub($size, 2), $size));
    }
}

$finalIncr = gmp_powm($increment, $iter, $size);
$finalOffs = gmp_sub(gmp_init(1), $finalIncr);
$invmod = gmp_powm(gmp_sub(gmp_init(1), $increment), gmp_sub($size, 2), $size);
$finalOffs = gmp_mul($finalOffs, $invmod);
$finalOffs = gmp_mul($finalOffs, $offset);

$answer = gmp_mul(gmp_init(2020), $finalIncr);
$answer = gmp_add($answer, $finalOffs);
$answer = gmp_mod($answer, $size);

echo gmp_strval($answer) . "\n";

fclose($file);
?>