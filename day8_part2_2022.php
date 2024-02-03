
<?php

$grid = [];
$handle = fopen("input.txt", "r");
if ($handle) {
    $y = 0;
    while (($line = fgets($handle)) !== false) {
        $line = str_split(trim($line));
        foreach ($line as $x => $b) {
            $grid["$x,$y"] = (int)$b;
        }
        $y++;
    }
    fclose($handle);
}

$neighbors4 = [[0, 1], [0, -1], [1, 0], [-1, 0]];

$maxScore = 0;
foreach ($grid as $p => $value) {
    $p = explode(",", $p);
    $score = 1;
    foreach ($neighbors4 as $n) {
        $next = $p;
        $view = 0;
        while (true) {
            $next[0] += $n[0];
            $next[1] += $n[1];
            $nextKey = implode(",", $next);
            if (array_key_exists($nextKey, $grid)) {
                $view++;
                if ($grid[$nextKey] >= $value) {
                    $score *= $view;
                    break;
                }
            } else {
                $score *= $view;
                break;
            }
        }
    }

    if ($score > $maxScore) {
        $maxScore = $score;
    }
}
echo $maxScore . PHP_EOL;
?>
