
<?php
$lines = file('input.txt', FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
$ranges = [];

foreach ($lines as $line) {
    $line = trim($line);
    if ($line === '') continue;
    if (preg_match('/^(-?\d+)\s*-\s*(-?\d+)$/', $line, $m)) {
        $a = (int)$m[1];
        $b = (int)$m[2];
        if ($a > $b) {
            $t = $a;
            $a = $b;
            $b = $t;
        }
        $ranges[] = [$a, $b];
    }
}

if (!$ranges) {
    echo "Total fresh IDs: 0\n";
    exit;
}

usort($ranges, fn($x, $y) => $x[0] <=> $y[0] ?: $x[1] <=> $y[1]);

$total = 0;
[$curMin, $curMax] = $ranges[0];

foreach (array_slice($ranges, 1) as $r) {
    [$min, $max] = $r;
    if ($min <= $curMax) {
        if ($max > $curMax) $curMax = $max;
    } else {
        $total += $curMax - $curMin + 1;
        $curMin = $min;
        $curMax = $max;
    }
}
$total += $curMax - $curMin + 1;

echo "Total fresh IDs: {$total}\n";
?>
