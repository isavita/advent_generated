<?php
$input = file_get_contents("input.txt");
$lines = explode("\n", trim($input));
$stars = [];

foreach ($lines as $line) {
    preg_match('/position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>/', $line, $matches);
    if (count($matches) === 5) {
        $stars[] = [
            'x' => (int)$matches[1],
            'y' => (int)$matches[2],
            'vX' => (int)$matches[3],
            'vY' => (int)$matches[4]
        ];
    }
}

$smallestT = 0;
$smallestArea = PHP_INT_MAX;

for ($t = 1; $t < 100000; $t++) {
    $maxX = $maxY = PHP_INT_MIN;
    $minX = $minY = PHP_INT_MAX;

    foreach ($stars as &$star) {
        $x = $star['x'] + $star['vX'] * $t;
        $y = $star['y'] + $star['vY'] * $t;

        $maxX = max($maxX, $x);
        $minX = min($minX, $x);
        $maxY = max($maxY, $y);
        $minY = min($minY, $y);
    }

    $area = ($maxX - $minX + 1) * ($maxY - $minY + 1);

    if ($smallestArea > $area) {
        $smallestArea = $area;
        $smallestT = $t;
    }
}

foreach ($stars as &$star) {
    $star['x'] += $star['vX'] * $smallestT;
    $star['y'] += $star['vY'] * $smallestT;
}

$maxX = $maxY = PHP_INT_MIN;
$minX = $minY = PHP_INT_MAX;

foreach ($stars as $star) {
    $maxX = max($maxX, $star['x']);
    $minX = min($minX, $star['x']);
    $maxY = max($maxY, $star['y']);
    $minY = min($minY, $star['y']);
}

$width = $maxX - $minX + 1;
$height = $maxY - $minY + 1;
$grid = array_fill(0, $height, array_fill(0, $width, ' '));

foreach ($stars as $star) {
    $grid[$star['y'] - $minY][$star['x'] - $minX] = '#';
}

foreach ($grid as $row) {
    echo implode('', $row) . "\n";
}
?>