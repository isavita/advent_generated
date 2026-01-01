
<?php
function bigAdd(string $a, string $b): string {
    $i = strlen($a) - 1;
    $j = strlen($b) - 1;
    $carry = 0;
    $res = '';
    while ($i >= 0 || $j >= 0 || $carry) {
        $da = $i >= 0 ? (ord($a[$i]) - 48) : 0;
        $db = $j >= 0 ? (ord($b[$j]) - 48) : 0;
        $sum = $da + $db + $carry;
        $carry = intdiv($sum, 10);
        $res = chr(($sum % 10) + 48) . $res;
        $i--; $j--;
    }
    return $res;
}
function bigMul(string $a, string $b): string {
    $al = strlen($a);
    $bl = strlen($b);
    $prod = array_fill(0, $al + $bl, 0);
    for ($i = $al - 1; $i >= 0; $i--) {
        for ($j = $bl - 1; $j >= 0; $j--) {
            $prod[$i + $j + 1] += (ord($a[$i]) - 48) * (ord($b[$j]) - 48);
        }
    }
    for ($k = $al + $bl - 1; $k > 0; $k--) {
        if ($prod[$k] > 9) {
            $prod[$k - 1] += intdiv($prod[$k], 10);
            $prod[$k] %= 10;
        }
    }
    $i = 0;
    while ($i < $al + $bl && $prod[$i] == 0) $i++;
    if ($i == $al + $bl) return '0';
    $s = '';
    for (; $i < $al + $bl; $i++) $s .= chr($prod[$i] + 48);
    return $s;
}
function isSep(int $col, array $lines, int $n): bool {
    for ($i = 0; $i < $n; $i++) {
        if ($col < strlen($lines[$i]) && !ctype_space($lines[$i][$col])) return false;
    }
    return true;
}
function processBlock(int $sc, int $ec, string &$gt, array $lines, int $n) {
    $nums = [];
    $op = 0;
    for ($i = 0; $i < $n; $i++) {
        $end = $ec + 1;
        if ($end > strlen($lines[$i])) $end = strlen($lines[$i]);
        if ($sc >= strlen($lines[$i])) continue;
        $seg = substr($lines[$i], $sc, $end - $sc);
        $seg = trim($seg);
        if ($seg === '') continue;
        if ($seg === '+') $op = 1;
        elseif ($seg === '*') $op = 2;
        else $nums[] = $seg;
    }
    $cnt = count($nums);
    if ($cnt == 0) return;
    if ($op == 1) {
        $acc = '0';
        foreach ($nums as $n) $acc = bigAdd($acc, $n);
    } elseif ($op == 2) {
        $acc = '1';
        foreach ($nums as $n) $acc = bigMul($acc, $n);
    } else {
        $acc = $cnt == 1 ? $nums[0] : '0';
    }
    $gt = bigAdd($gt, $acc);
}
$lines = file('input.txt', FILE_IGNORE_NEW_LINES);
$nlines = count($lines);
$maxw = 0;
foreach ($lines as $ln) {
    $len = strlen($ln);
    if ($len > $maxw) $maxw = $len;
}
$grand = '0';
$inb = false;
$sc = 0;
for ($x = 0; $x < $maxw; $x++) {
    if (!isSep($x, $lines, $nlines)) {
        if (!$inb) { $inb = true; $sc = $x; }
    } else {
        if ($inb) { processBlock($sc, $x - 1, $grand, $lines, $nlines); $inb = false; }
    }
}
if ($inb) processBlock($sc, $maxw - 1, $grand, $lines, $nlines);
echo "Grand total: $grand\n";
?>
