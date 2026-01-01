
<?php
function bits(int $x): int {
    $c = 0;
    while ($x) {
        $x &= ($x - 1);
        $c++;
    }
    return $c;
}

function gaussianMinWeight(array &$mat, int $R, int $C): int {
    $colIsPivot = array_fill(0, $C, 0);
    $pivotRow = 0;
    for ($c = 0; $c < $C && $pivotRow < $R; $c++) {
        $sel = -1;
        for ($r = $pivotRow; $r < $R; $r++) {
            if ($mat[$r][$c] == 1) { $sel = $r; break; }
        }
        if ($sel == -1) continue;
        $tmp = $mat[$pivotRow];
        $mat[$pivotRow] = $mat[$sel];
        $mat[$sel] = $tmp;
        for ($r = 0; $r < $R; $r++) {
            if ($r != $pivotRow && $mat[$r][$c] == 1) {
                for ($k = $c; $k <= $C; $k++) {
                    $mat[$r][$k] ^= $mat[$pivotRow][$k];
                }
            }
        }
        $colIsPivot[$c] = 1;
        $pivotRow++;
    }
    for ($r = $pivotRow; $r < $R; $r++) {
        if ($mat[$r][$C] == 1) return -1;
    }
    $free = [];
    for ($c = 0; $c < $C; $c++) {
        if (!$colIsPivot[$c]) $free[] = $c;
    }
    $nFree = count($free);
    $min = PHP_INT_MAX;
    $limit = 1 << $nFree;
    for ($i = 0; $i < $limit; $i++) {
        $x = array_fill(0, $C, 0);
        $cw = bits($i);
        for ($j = 0; $j < $nFree; $j++) {
            if (($i >> $j) & 1) $x[$free[$j]] = 1;
        }
        $curr = 0;
        for ($c = 0; $c < $C; $c++) {
            if ($colIsPivot[$c]) {
                $val = $mat[$curr][$C];
                for ($k = $c + 1; $k < $C; $k++) {
                    if ($mat[$curr][$k] == 1) $val ^= $x[$k];
                }
                $x[$c] = $val;
                if ($val) $cw++;
                $curr++;
            }
        }
        if ($cw < $min) $min = $cw;
    }
    return $min;
}

$total = 0;
$fh = fopen('input.txt', 'r');
if (!$fh) { fwrite(STDERR, "Cannot open input.txt\n"); exit(1); }

while (($line = fgets($fh)) !== false) {
    $line = trim($line);
    $lb = strpos($line, '[');
    if ($lb === false) continue;
    $rb = strpos($line, ']', $lb);
    if ($rb === false) continue;
    $targetStr = substr($line, $lb + 1, $rb - $lb - 1);
    $R = strlen($targetStr);
    $target = [];
    for ($i = 0; $i < $R; $i++) $target[$i] = ($targetStr[$i] === '#') ? 1 : 0;

    $buttons = [];
    $pos = $rb + 1;
    while (true) {
        $op = strpos($line, '(', $pos);
        if ($op === false) break;
        $cl = strpos($line, ')', $op);
        if ($cl === false) break;
        $content = substr($line, $op + 1, $cl - $op - 1);
        $parts = array_filter(array_map('trim', explode(',', $content)), 'strlen');
        $btn = [];
        foreach ($parts as $p) $btn[] = (int)$p;
        $buttons[] = $btn;
        $pos = $cl + 1;
    }
    $C = count($buttons);
    $mat = array_fill(0, $R, array_fill(0, $C + 1, 0));
    for ($r = 0; $r < $R; $r++) {
        for ($c = 0; $c < $C; $c++) {
            foreach ($buttons[$c] as $idx) {
                if ($idx == $r) { $mat[$r][$c] = 1; break; }
            }
        }
        $mat[$r][$C] = $target[$r];
    }

    $mw = gaussianMinWeight($mat, $R, $C);
    if ($mw != -1) $total += $mw;
}
fclose($fh);
echo $total . PHP_EOL;
?>
