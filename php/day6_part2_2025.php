
<?php
function addStr(string $a, string $b): string {
    $i = strlen($a) - 1;
    $j = strlen($b) - 1;
    $carry = 0;
    $res = '';
    while ($i >= 0 || $j >= 0 || $carry) {
        $da = $i >= 0 ? ord($a[$i]) - 48 : 0;
        $db = $j >= 0 ? ord($b[$j]) - 48 : 0;
        $sum = $da + $db + $carry;
        $res = chr(($sum % 10) + 48) . $res;
        $carry = intdiv($sum, 10);
        $i--; $j--;
    }
    return $res;
}
function mulStr(string $a, string $b): string {
    if ($a === '0' || $b === '0') return '0';
    $la = strlen($a);
    $lb = strlen($b);
    $tmp = array_fill(0, $la + $lb, 0);
    for ($i = 0; $i < $la; $i++) {
        $da = ord($a[$la - 1 - $i]) - 48;
        for ($j = 0; $j < $lb; $j++) {
            $db = ord($b[$lb - 1 - $j]) - 48;
            $tmp[$i + $j] += $da * $db;
        }
    }
    $carry = 0;
    for ($k = 0; $k < $la + $lb; $k++) {
        $sum = $tmp[$k] + $carry;
        $tmp[$k] = $sum % 10;
        $carry = intdiv($sum, 10);
    }
    $len = $la + $lb;
    while ($len > 1 && $tmp[$len - 1] === 0) $len--;
    $res = '';
    for ($i = $len - 1; $i >= 0; $i--) $res .= chr($tmp[$i] + 48);
    return $res;
}
function processBlock(array $lines, int $lineCnt, int $start, int $end, string &$grandTotal): void {
    $nums = [];
    $op = '+';
    for ($c = $start; $c <= $end; $c++) {
        $buf = '';
        for ($r = 0; $r < $lineCnt; $r++) {
            $ln = $lines[$r];
            if ($c < strlen($ln)) {
                $ch = $ln[$c];
                if ($ch >= '0' && $ch <= '9') $buf .= $ch;
                elseif ($ch === '+' || $ch === '*') $op = $ch;
            }
        }
        if ($buf !== '') $nums[] = $buf;
    }
    if (empty($nums)) return;
    if ($op === '*') {
        $blockRes = '1';
        foreach ($nums as $n) $blockRes = mulStr($blockRes, $n);
    } else {
        $blockRes = '0';
        foreach ($nums as $n) $blockRes = addStr($blockRes, $n);
    }
    $grandTotal = addStr($grandTotal, $blockRes);
}
$lines = [];
$maxW = 0;
$fh = fopen('input.txt', 'r');
if ($fh) {
    while (($line = fgets($fh)) !== false) {
        $line = rtrim($line, "\r\n");
        $lines[] = $line;
        $len = strlen($line);
        if ($len > $maxW) $maxW = $len;
    }
    fclose($fh);
}
$lineCnt = count($lines);
if ($lineCnt === 0) {
    echo "Grand total: 0\n";
    exit;
}
$isSep = array_fill(0, $maxW, true);
for ($x = 0; $x < $maxW; $x++) {
    for ($r = 0; $r < $lineCnt; $r++) {
        $ln = $lines[$r];
        if ($x < strlen($ln) && !ctype_space($ln[$x])) {
            $isSep[$x] = false;
            break;
        }
    }
}
$grandTotal = '0';
$inBlock = false;
$start = 0;
for ($x = 0; $x < $maxW; $x++) {
    if (!$isSep[$x]) {
        if (!$inBlock) {$inBlock = true; $start = $x;}
    } else {
        if ($inBlock) {
            processBlock($lines, $lineCnt, $start, $x - 1, $grandTotal);
            $inBlock = false;
        }
    }
}
if ($inBlock) processBlock($lines, $lineCnt, $start, $maxW - 1, $grandTotal);
echo "Grand total: $grandTotal\n";
