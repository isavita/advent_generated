
<?php

function mod(int $a, int $b): int {
    return ($a % $b + $b) % $b;
}

function parseLine(string $line): array {
    preg_match('/p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)/', $line, $matches);
    if (count($matches) !== 5) {
        throw new Exception("Invalid line format: $line");
    }
    return [
        'x' => (int) $matches[1],
        'y' => (int) $matches[2],
        'vx' => (int) $matches[3],
        'vy' => (int) $matches[4],
    ];
}

function moveRobots(array &$robots, int $sizeX, int $sizeY): void {
    foreach ($robots as &$robot) {
        $robot['x'] = mod($robot['x'] + $robot['vx'], $sizeX);
        $robot['y'] = mod($robot['y'] + $robot['vy'], $sizeY);
    }
}

function countQuadrants(array $robots, int $sizeX, int $sizeY): array {
    $counts = [0, 0, 0, 0];
    $centerX = (int)($sizeX / 2);
    $centerY = (int)($sizeY / 2);

    foreach ($robots as $robot) {
        $x = $robot['x'];
        $y = $robot['y'];
        if ($x < $centerX) {
            if ($y < $centerY) {
                $counts[0]++;
            } else if ($y > $centerY) {
                $counts[1]++;
            }
        } else if ($x > $centerX) {
            if ($y < $centerY) {
                $counts[2]++;
            } else if ($y > $centerY) {
                $counts[3]++;
            }
        }
    }
    return $counts;
}

function hasNoOverlaps(array $robots): bool {
    $positionMap = [];
    foreach ($robots as $robot) {
        $pos = [$robot['x'], $robot['y']];
        if (isset($positionMap[implode(',', $pos)])) {
            return false;
        }
        $positionMap[implode(',', $pos)] = true;
    }
    return true;
}

function drawGrid(array $robots, int $sizeX, int $sizeY): void {
    $gridMap = [];
    foreach ($robots as $robot) {
        $gridMap[implode(',', [$robot['x'], $robot['y']])] = true;
    }

    for ($y = 0; $y < $sizeY; $y++) {
        $line = "";
        for ($x = 0; $x < $sizeX; $x++) {
            if (isset($gridMap[implode(',', [$x, $y])])) {
                $line .= "#";
            } else {
                $line .= ".";
            }
        }
        echo $line . "\n";
    }
}

$sizeX = 101;
$sizeY = 103;
$robots = [];

$file = fopen("input.txt", "r");
if ($file) {
    while (($line = fgets($file)) !== false) {
        $line = trim($line);
        if ($line === "") {
            continue;
        }
        try {
            $robots[] = parseLine($line);
        } catch (Exception $e) {
            fwrite(STDERR, "Error parsing line: " . $e->getMessage() . "\n");
            exit(1);
        }
    }
    fclose($file);
} else {
    fwrite(STDERR, "Error opening input.txt\n");
    exit(1);
}

$robotsPart1 = $robots;
for ($n = 0; $n < 100; $n++) {
    moveRobots($robotsPart1, $sizeX, $sizeY);
}

$counts = countQuadrants($robotsPart1, $sizeX, $sizeY);
$safetyFactor = 1;
foreach ($counts as $c) {
    $safetyFactor *= $c;
}
echo "Part 1 - Safety Factor after 100 seconds: " . $safetyFactor . "\n";

$robotsPart2 = $robots;
$seconds = 0;
while (true) {
    if (hasNoOverlaps($robotsPart2)) {
        break;
    }
    moveRobots($robotsPart2, $sizeX, $sizeY);
    $seconds++;
    if ($seconds > 1000000) {
        echo "Exceeded maximum iterations without finding a unique position configuration.\n";
        exit(1);
    }
}
echo "Part 2 - Fewest seconds to display Easter egg: " . $seconds . "\n";
echo "Final positions of robots:\n";
drawGrid($robotsPart2, $sizeX, $sizeY);
?>
