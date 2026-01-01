
<?php
function floorDiv(int $a, int $b): int {
    $q = intdiv($a, $b);
    if ($a < 0 && $a % $b !== 0) $q--;
    return $q;
}
$f = fopen('input.txt', 'r');
$pos = 50;
$hits = 0;
while (fscanf($f, " %c%d ", $dir, $amt) === 2) {
    if ($dir === 'R') {
        $new = $pos + $amt;
        $hits += intdiv($new, 100);
        $pos = $new % 100;
    } else {
        $hits += floorDiv($pos - 1, 100) - floorDiv($pos - $amt - 1, 100);
        $pos = ($pos - $amt) % 100;
        if ($pos < 0) $pos += 100;
    }
}
fclose($f);
echo $hits . PHP_EOL;
?>
