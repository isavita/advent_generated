
<?php
$grid = [];
$moves = '';
$readingMap = true;

$file = fopen("input.txt", "r");
if ($file) {
    while (($line = fgets($file)) !== false) {
        $line = rtrim($line, "\r\n");
        if ($readingMap) {
            if (strpos($line, '#') !== false) {
                $grid[] = $line;
            } else {
                $readingMap = false;
                $moves .= $line;
            }
        } else {
            $moves .= $line;
        }
    }
    fclose($file);
}

$rows = count($grid);
$cols = strlen($grid[0]);
$runes = array_map(function($row) { return str_split($row); }, $grid);

$robotR = 0;
$robotC = 0;
for ($r = 0; $r < $rows; $r++) {
    for ($c = 0; $c < $cols; $c++) {
        if ($runes[$r][$c] == '@') {
            $robotR = $r;
            $robotC = $c;
            break 2;
        }
    }
}

$dirs = [
    '^' => [-1, 0],
    'v' => [1, 0],
    '<' => [0, -1],
    '>' => [0, 1],
];

$movesLen = strlen($moves);
for ($i = 0; $i < $movesLen; $i++) {
    $move = $moves[$i];
    $d = $dirs[$move];
    $nr = $robotR + $d[0];
    $nc = $robotC + $d[1];
    if ($runes[$nr][$nc] == '#') {
        continue;
    } else if ($runes[$nr][$nc] == 'O') {
        if (!pushBoxes($runes, $nr, $nc, $d[0], $d[1], $rows, $cols)) {
            continue;
        }
    }
    if ($runes[$nr][$nc] == '.' || $runes[$nr][$nc] == 'O') {
        $runes[$robotR][$robotC] = '.';
        $runes[$nr][$nc] = '@';
        $robotR = $nr;
        $robotC = $nc;
    }
}

$sum = 0;
for ($r = 0; $r < $rows; $r++) {
    for ($c = 0; $c < $cols; $c++) {
        if ($runes[$r][$c] == 'O') {
            $sum += $r * 100 + $c;
        }
    }
}

echo $sum . PHP_EOL;

function pushBoxes(&$runes, $r, $c, $dr, $dc, $rows, $cols) {
    $nr = $r + $dr;
    $nc = $c + $dc;
    if ($runes[$nr][$nc] == '#') {
        return false;
    }
    if ($runes[$nr][$nc] == 'O') {
        if (!pushBoxes($runes, $nr, $nc, $dr, $dc, $rows, $cols)) {
            return false;
        }
    }
    if ($runes[$nr][$nc] == '.') {
        $runes[$nr][$nc] = 'O';
        $runes[$r][$c] = '.';
        return true;
    }
    return false;
}
?>
