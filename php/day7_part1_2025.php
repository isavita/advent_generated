<?php
$lines = file('input.txt', FILE_IGNORE_NEW_LINES);
$height = count($lines);
if ($height === 0) { exit; }
$width = strlen($lines[0]);
$grid = $lines;

$sx = $sy = 0;
for ($y = 0; $y < $height; $y++) {
    $pos = strpos($grid[$y], 'S');
    if ($pos !== false) {
        $sx = $pos;
        $sy = $y;
        break;
    }
}

$active = array_fill(0, $width, 0);
$active[$sx] = 1;
$splits = 0;

for ($y = $sy; $y < $height; $y++) {
    $next = array_fill(0, $width, 0);
    for ($x = 0; $x < $width; $x++) {
        if (!$active[$x]) continue;
        if ($grid[$y][$x] === '^') {
            $splits++;
            if ($x > 0) $next[$x - 1] = 1;
            if ($x + 1 < $width) $next[$x + 1] = 1;
        } else {
            $next[$x] = 1;
        }
    }
    $active = $next;
    $any = false;
    foreach ($active as $v) {
        if ($v) { $any = true; break; }
    }
    if (!$any) break;
}
echo $splits . PHP_EOL;
?>