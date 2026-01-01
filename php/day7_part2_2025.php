<?php
$lines = file('input.txt', FILE_IGNORE_NEW_LINES);
$height = count($lines);
if ($height === 0) {
    echo "0\n";
    exit;
}
$width = strlen($lines[0]);
$startX = $startY = -1;
for ($y = 0; $y < $height; $y++) {
    $pos = strpos($lines[$y], 'S');
    if ($pos !== false) {
        $startX = $pos;
        $startY = $y;
        break;
    }
}
if ($startX === -1) {
    fwrite(STDERR, "Start point 'S' not found\n");
    exit(1);
}
$counts = [$startX => 1];
for ($y = $startY; $y < $height; $y++) {
    $next = [];
    $row = $lines[$y];
    foreach ($counts as $x => $c) {
        $isSplitter = ($x >= 0 && $x < $width && $row[$x] === '^');
        if ($isSplitter) {
            foreach ([$x - 1, $x + 1] as $k) {
                $next[$k] = ($next[$k] ?? 0) + $c;
            }
        } else {
            $next[$x] = ($next[$x] ?? 0) + $c;
        }
    }
    $counts = $next;
}
$total = array_sum($counts);
echo $total . PHP_EOL;
?>