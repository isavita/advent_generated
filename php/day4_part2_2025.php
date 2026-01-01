
<?php
$lines = @file('input.txt', FILE_IGNORE_NEW_LINES);
if ($lines === false || count($lines) === 0) {
    echo "Total rolls removed: 0\n";
    exit;
}
$R = count($lines);
$C = 0;
foreach ($lines as $ln) {
    $len = strlen($ln);
    if ($len > $C) $C = $len;
}
$grid = [];
for ($r = 0; $r < $R; ++$r) {
    $row = str_split($lines[$r]);
    $row = array_pad($row, $C, '.');
    $grid[$r] = $row;
}
$removed = 0;
$dr = [-1,-1,-1,0,0,1,1,1];
$dc = [-1,0,1,-1,1,-1,0,1];
do {
    $changed = false;
    for ($r = 0; $r < $R; ++$r) {
        for ($c = 0; $c < $C; ++$c) {
            if ($grid[$r][$c] !== '@') continue;
            $cnt = 0;
            for ($k = 0; $k < 8; ++$k) {
                $nr = $r + $dr[$k];
                $nc = $c + $dc[$k];
                if ($nr >= 0 && $nr < $R && $nc >= 0 && $nc < $C && $grid[$nr][$nc] === '@')
                    ++$cnt;
            }
            if ($cnt < 4) {
                $grid[$r][$c] = '*';
                $changed = true;
            }
        }
    }
    for ($r = 0; $r < $R; ++$r) {
        for ($c = 0; $c < $C; ++$c) {
            if ($grid[$r][$c] === '*') {
                $grid[$r][$c] = '.';
                ++$removed;
            }
        }
    }
} while ($changed);
echo "Total rolls removed: $removed\n";
?>
