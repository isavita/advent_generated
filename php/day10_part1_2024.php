
<?php
$lines = explode("\n", trim(file_get_contents("input.txt")));
$nr = count($lines);
$nc = strlen($lines[0]);
$grid = [];
for ($i = 0; $i < $nr; $i++) {
    $grid[] = array_map('intval', str_split($lines[$i]));
}

$dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]];
$trailheads = [];
for ($r = 0; $r < $nr; $r++) {
    for ($c = 0; $c < $nc; $c++) {
        if ($grid[$r][$c] == 0) {
            $trailheads[] = [$r, $c];
        }
    }
}

$sumScores = 0;
foreach ($trailheads as $th) {
    $reached = [];
    $front = [[$th[0], $th[1], 0]];
    $visited = [];
    while (!empty($front)) {
        $cur = array_pop($front);
        $r = $cur[0];
        $c = $cur[1];
        $h = $cur[2];
        if ($h == 9) {
            $reached[$r . "," . $c] = true;
            continue;
        }
        foreach ($dirs as $d) {
            $nr2 = $r + $d[0];
            $nc2 = $c + $d[1];
            if ($nr2 < 0 || $nr2 >= $nr || $nc2 < 0 || $nc2 >= $nc) continue;
            if ($grid[$nr2][$nc2] == $h + 1) {
                $key = $nr2 . "," . $nc2 . "," . ($h + 1);
                if (!isset($visited[$key])) {
                    $visited[$key] = true;
                    $front[] = [$nr2, $nc2, $h + 1];
                }
            }
        }
    }
    $sumScores += count($reached);
}

echo $sumScores . PHP_EOL;
?>
