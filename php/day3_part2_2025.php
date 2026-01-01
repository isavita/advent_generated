
<?php
function addBig(string $a, string $b): string
{
    $i = strlen($a) - 1;
    $j = strlen($b) - 1;
    $carry = 0;
    $res = '';
    while ($i >= 0 || $j >= 0 || $carry) {
        $d1 = $i >= 0 ? ord($a[$i]) - 48 : 0;
        $d2 = $j >= 0 ? ord($b[$j]) - 48 : 0;
        $sum = $d1 + $d2 + $carry;
        $res = chr(($sum % 10) + 48) . $res;
        $carry = intdiv($sum, 10);
        $i--; $j--;
    }
    return $res;
}

const TARGET = 12;
$total = '0';

$fh = fopen('input.txt', 'r');
if (!$fh) {
    exit(1);
}
while (($line = fgets($fh)) !== false) {
    $line = rtrim($line, "\r\n");
    // strip trailing non‑digits
    while ($line !== '' && ($c = $line[strlen($line) - 1]) < '0' || $c > '9') {
        $line = substr($line, 0, -1);
    }
    $len = strlen($line);
    if ($len < TARGET) {
        continue;
    }

    $rem = $len - TARGET;
    $stack = [];

    for ($i = 0; $i < $len; $i++) {
        $digit = $line[$i];
        while ($rem > 0 && $stack && end($stack) < $digit) {
            array_pop($stack);
            $rem--;
        }
        $stack[] = $digit;
    }

    $num = implode('', array_slice($stack, 0, TARGET));
    $total = addBig($total, $num);
}
fclose($fh);

echo $total . PHP_EOL;
?>
