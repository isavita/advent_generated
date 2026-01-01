
<?php
function calc(string $s): int {
    $len = strlen($s);
    for ($d1 = 9; $d1 >= 0; --$d1) {
        $pos = strpos($s, (string)$d1);
        if ($pos === false || $pos >= $len - 1) continue;
        $max2 = -1;
        for ($i = $pos + 1; $i < $len; ++$i) {
            $c = $s[$i];
            if ($c >= '0' && $c <= '9') {
                $v = ord($c) - 48;
                if ($v > $max2) $max2 = $v;
                if ($max2 === 9) break;
            }
        }
        if ($max2 !== -1) return $d1 * 10 + $max2;
    }
    return 0;
}

$total = 0;
$handle = fopen('input.txt', 'r');
if ($handle) {
    while (($line = fgets($handle)) !== false) {
        $line = rtrim($line, "\r\n");
        $total += calc($line);
    }
    fclose($handle);
}
echo $total . PHP_EOL;
?>
