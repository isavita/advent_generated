
<?php

// Read input from file
$content = file_get_contents("input.txt");

// Parse coordinates
$coordinates = parseCoordinates($content);

// Find the region size
$regionSize = findRegionSize($coordinates, 10000);

// Print the result
echo $regionSize . PHP_EOL;

function parseCoordinates($input) {
    $lines = explode("\n", trim($input));
    $coordinates = [];

    foreach ($lines as $line) {
        sscanf($line, "%d, %d", $x, $y);
        $coordinates[] = ['x' => $x, 'y' => $y];
    }

    return $coordinates;
}

function findRegionSize($coordinates, $maxDistance) {
    list($minX, $minY, $maxX, $maxY) = findBoundingBox($coordinates);
    $regionSize = 0;

    for ($x = $minX; $x <= $maxX; $x++) {
        for ($y = $minY; $y <= $maxY; $y++) {
            $totalDistance = 0;

            foreach ($coordinates as $c) {
                $totalDistance += manhattanDistance($x, $y, $c['x'], $c['y']);
            }

            if ($totalDistance < $maxDistance) {
                $regionSize++;
            }
        }
    }

    return $regionSize;
}

function findBoundingBox($coordinates) {
    $minX = PHP_INT_MAX;
    $minY = PHP_INT_MAX;
    $maxX = PHP_INT_MIN;
    $maxY = PHP_INT_MIN;

    foreach ($coordinates as $c) {
        $minX = min($minX, $c['x']);
        $minY = min($minY, $c['y']);
        $maxX = max($maxX, $c['x']);
        $maxY = max($maxY, $c['y']);
    }

    return [$minX, $minY, $maxX, $maxY];
}

function manhattanDistance($x1, $y1, $x2, $y2) {
    return abs($x1 - $x2) + abs($y1 - $y2);
}
