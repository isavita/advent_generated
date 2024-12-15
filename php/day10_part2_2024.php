
<?php
$lines = explode("\n", trim(file_get_contents("input.txt")));
$nr = count($lines);
$nc = strlen($lines[0]);
$grid = [];
for ($i = 0; $i < $nr; $i++) {
    $grid[$i] = array_map('intval', str_split($lines[$i]));
}

$dp = array_fill(0, $nr, array_fill(0, $nc, -1));
$dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]];

function dfs(int $r, int $c, array $grid, array &$dp, array $dirs, int $nr, int $nc): int {
    if ($dp[$r][$c] !== -1) {
        return $dp[$r][$c];
    }
    $h = $grid[$r][$c];
    if ($h === 9) {
        $dp[$r][$c] = 1;
        return 1;
    }
    $sum = 0;
    foreach ($dirs as $d) {
        $nr2 = $r + $d[0];
        $nc2 = $c + $d[1];
        if ($nr2 < 0 || $nr2 >= $nr || $nc2 < 0 || $nc2 >= $nc) {
            continue;
        }
        if ($grid[$nr2][$nc2] === $h + 1) {
            $sum += dfs($nr2, $nc2, $grid, $dp, $dirs, $nr, $nc);
        }
    }
    $dp[$r][$c] = $sum;
    return $sum;
}

$total = 0;
for ($r = 0; $r < $nr; $r++) {
    for ($c = 0; $c < $nc; $c++) {
        if ($grid[$r][$c] === 0) {
            $total += dfs($r, $c, $grid, $dp, $dirs, $nr, $nc);
        }
    }
}
echo $total . PHP_EOL;
?>
