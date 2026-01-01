
<?php
function is_invalid(int $x): bool {
    $s = (string)$x;
    $n = strlen($s);
    if ($n <= 1) return false;
    for ($p = 1; $p <= intdiv($n, 2); $p++) {
        if ($n % $p) continue;
        $ok = true;
        for ($i = $p; $i < $n; $i++) {
            if ($s[$i] !== $s[$i % $p]) { $ok = false; break; }
        }
        if ($ok) return true;
    }
    return false;
}

$buf = @file_get_contents('input.txt');
if ($buf === false) exit(1);

$sum = 0;
preg_match_all('/(\d+)-(\d+)/', $buf, $m, PREG_SET_ORDER);
foreach ($m as $pair) {
    $a = (int)$pair[1];
    $b = (int)$pair[2];
    if ($a > $b) { $t = $a; $a = $b; $b = $t; }
    for ($x = $a; $x <= $b; $x++) {
        if (is_invalid($x)) $sum += $x;
        if ($x === PHP_INT_MAX) break;
    }
}
echo $sum . PHP_EOL;
