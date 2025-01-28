
<?php

function readBytePositionsFromFile(string $filename): array
{
    $bytePositions = [];
    $file = fopen($filename, 'r');
    if ($file) {
        while (($line = fgets($file)) !== false) {
            $coords = explode(',', trim($line));
            if (count($coords) === 2) {
                $bytePositions[] = [(int)$coords[0], (int)$coords[1]];
            }
        }
        fclose($file);
    } else {
        die("Unable to open input file: $filename");
    }
    return $bytePositions;
}

function createGrid(int $size): array
{
    return array_fill(0, $size, array_fill(0, $size, 0));
}

function markCorrupted(array &$grid, array $bytePositions): void
{
    foreach ($bytePositions as $pos) {
        $x = $pos[0];
        $y = $pos[1];
        if (isset($grid[$y][$x])) {
            $grid[$y][$x] = 1; // 1 represents corrupted
        }
    }
}

function bfs(array $grid, int $size, array $start, array $end): int
{
    if ($grid[$start[1]][$start[0]] == 1 || $grid[$end[1]][$end[0]] == 1) {
        return -1; // Start or end is blocked
    }

    $queue = [[$start, 0]]; // [position, distance]
    $visited = array_fill(0, $size, array_fill(0, $size, false));
    $visited[$start[1]][$start[0]] = true;

    $directions = [[0, 1], [0, -1], [1, 0], [-1, 0]]; // Down, Up, Right, Left

    while (!empty($queue)) {
        [$currentPos, $distance] = array_shift($queue);
        if ($currentPos == $end) {
            return $distance;
        }

        foreach ($directions as $dir) {
            $nextPos = [$currentPos[0] + $dir[0], $currentPos[1] + $dir[1]];
            if (
                $nextPos[0] >= 0 && $nextPos[0] < $size &&
                $nextPos[1] >= 0 && $nextPos[1] < $size &&
                !$visited[$nextPos[1]][$nextPos[0]] &&
                $grid[$nextPos[1]][$nextPos[0]] == 0
            ) {
                $visited[$nextPos[1]][$nextPos[0]] = true;
                $queue[] = [$nextPos, $distance + 1];
            }
        }
    }

    return -1; // No path found
}

function solvePart1(array $bytePositions): int
{
    $gridSize = 71;
    $grid = createGrid($gridSize);
    $bytesToSimulate = array_slice($bytePositions, 0, 1024);
    markCorrupted($grid, $bytesToSimulate);

    $start = [0, 0];
    $end = [$gridSize - 1, $gridSize - 1];
    return bfs($grid, $gridSize, $start, $end);
}

function solvePart2(array $bytePositions): string
{
    $gridSize = 71;
    for ($i = 1; $i <= count($bytePositions); ++$i) {
        $grid = createGrid($gridSize);
        $bytesSoFar = array_slice($bytePositions, 0, $i);
        markCorrupted($grid, $bytesSoFar);

        $start = [0, 0];
        $end = [$gridSize - 1, $gridSize - 1];
        $pathLength = bfs($grid, $gridSize, $start, $end);
        if ($pathLength == -1) {
            $blockingByte = $bytePositions[$i - 1];
            return $blockingByte[0] . "," . $blockingByte[1];
        }
    }
    return "No blocking byte found within the input."; // Should not happen based on problem description
}

$bytePositions = readBytePositionsFromFile('input.txt');

// Part 1
$part1Result = solvePart1($bytePositions);
echo $part1Result . "\n";

// Part 2
$part2Result = solvePart2($bytePositions);
echo $part2Result . "\n";

?>
