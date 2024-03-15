<?php
function fromSnafu($s) {
    $n = 0;
    for ($i = 0; $i < strlen($s); $i++) {
        $n *= 5;
        switch ($s[$i]) {
            case '=':
                $n -= 2;
                break;
            case '-':
                $n--;
                break;
            default:
                $n += ord($s[$i]) - ord('0');
                break;
        }
    }
    return $n;
}

function toSnafu($n) {
    $b = [];
    while ($n > 0) {
        switch ($n % 5) {
            case 3:
                $n += 5;
                $b[] = '=';
                break;
            case 4:
                $n += 5;
                $b[] = '-';
                break;
            default:
                $b[] = chr(ord('0') + $n % 5);
                break;
        }
        $n = (int)($n / 5);
    }
    return strrev(implode('', $b));
}

$sum = 0;
$file = fopen("input.txt", "r");
while ($line = fgets($file)) {
    $sum += fromSnafu(trim($line));
}
fclose($file);
echo toSnafu($sum) . "\n";
?>