
<?php
ini_set('memory_limit', '512M');

$grid = array_map('str_split', file("input.txt", FILE_IGNORE_NEW_LINES));
$n = count($grid);
$m = count($grid[0]);

$sx = -1;
$sy = -1;
$ex = -1;
$ey = -1;

for ($i = 0; $i < $n; $i++) {
    for ($j = 0; $j < $m; $j++) {
        if ($grid[$i][$j] == 'S') {
            $sx = $i;
            $sy = $j;
        } elseif ($grid[$i][$j] == 'E') {
            $ex = $i;
            $ey = $j;
        }
    }
}

$dx = [-1, 0, 1, 0];
$dy = [0, 1, 0, -1];

$dist = array_fill(0, $n, array_fill(0, $m, array_fill(0, 4, PHP_INT_MAX)));
$dist[$sx][$sy][1] = 0;

$pq = new SplPriorityQueue();
$pq->insert(['x' => $sx, 'y' => $sy, 'd' => 1, 'cost' => 0], 0);

while (!$pq->isEmpty()) {
    $u = $pq->extract();
    if ($dist[$u['x']][$u['y']][$u['d']] < $u['cost']) {
        continue;
    }
    if ($u['x'] == $ex && $u['y'] == $ey) {
        continue;
    }

    foreach ([(($u['d'] + 1) % 4), (($u['d'] + 3) % 4)] as $ndir) {
        $nc = $u['cost'] + 1000;
        if ($nc < $dist[$u['x']][$u['y']][$ndir]) {
            $dist[$u['x']][$u['y']][$ndir] = $nc;
            $pq->insert(['x' => $u['x'], 'y' => $u['y'], 'd' => $ndir, 'cost' => $nc], -$nc);
        }
    }

    $nx = $u['x'] + $dx[$u['d']];
    $ny = $u['y'] + $dy[$u['d']];

    if ($nx >= 0 && $nx < $n && $ny >= 0 && $ny < $m && $grid[$nx][$ny] != '#') {
        $nc = $u['cost'] + 1;
        if ($nc < $dist[$nx][$ny][$u['d']]) {
            $dist[$nx][$ny][$u['d']] = $nc;
            $pq->insert(['x' => $nx, 'y' => $ny, 'd' => $u['d'], 'cost' => $nc], -$nc);
        }
    }
}

$best = PHP_INT_MAX;
for ($d = 0; $d < 4; $d++) {
    $best = min($best, $dist[$ex][$ey][$d]);
}

$used = array_fill(0, $n, array_fill(0, $m, false));
$rev = [];
for ($d = 0; $d < 4; $d++) {
    if ($dist[$ex][$ey][$d] == $best) {
        $rev[] = ['x' => $ex, 'y' => $ey, 'd' => $d];
    }
}

$vis = array_fill(0, $n, array_fill(0, $m, array_fill(0, 4, false)));
foreach ($rev as $s) {
    $vis[$s['x']][$s['y']][$s['d']] = true;
}

while (!empty($rev)) {
    $u = array_pop($rev);
    $used[$u['x']][$u['y']] = true;
    $costU = $dist[$u['x']][$u['y']][$u['d']];

    foreach ([($u['d'] + 1) % 4, ($u['d'] + 3) % 4] as $pd) {
        if ($dist[$u['x']][$u['y']][$pd] == $costU - 1000) {
            if (!$vis[$u['x']][$u['y']][$pd]) {
                $vis[$u['x']][$u['y']][$pd] = true;
                $rev[] = ['x' => $u['x'], 'y' => $u['y'], 'd' => $pd];
            }
        }
    }

    $px = $u['x'] - $dx[$u['d']];
    $py = $u['y'] - $dy[$u['d']];

    if ($px >= 0 && $px < $n && $py >= 0 && $py < $m && $grid[$px][$py] != '#') {
        if ($dist[$px][$py][$u['d']] == $costU - 1) {
            if (!$vis[$px][$py][$u['d']]) {
                $vis[$px][$py][$u['d']] = true;
                $rev[] = ['x' => $px, 'y' => $py, 'd' => $u['d']];
            }
        }
    }
}

$cnt = 0;
for ($i = 0; $i < $n; $i++) {
    for ($j = 0; $j < $m; $j++) {
        if ($used[$i][$j] && $grid[$i][$j] != '#') {
            $cnt++;
        }
    }
}

echo $cnt . "\n";
?>
