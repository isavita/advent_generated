
<?php

const Open = '.';
const Trees = '|';
const Lumberyard = '#';
const Size = 50;

function main() {
    $grid = readInput("input.txt");
    $seenStates = [];
    $cycleStart = 0;
    $cycleLength = 0;
    for ($minute = 0; ; $minute++) {
        $state = gridToString($grid);
        if (array_key_exists($state, $seenStates)) {
            $cycleStart = $seenStates[$state];
            $cycleLength = $minute - $seenStates[$state];
            break;
        }
        $seenStates[$state] = $minute;
        $grid = transform($grid);
    }

    $remainingMinutes = (1000000000 - $cycleStart) % $cycleLength;
    for ($i = 0; $i < $remainingMinutes; $i++) {
        $grid = transform($grid);
    }

    list($wooded, $lumberyards) = countResources($grid);
    echo $wooded * $lumberyards . PHP_EOL;
}

function readInput($filename) {
    $file = fopen($filename, "r");
    $grid = [];
    while (!feof($file)) {
        $line = trim(fgets($file));
        $row = str_split($line);
        $grid[] = $row;
    }
    fclose($file);
    return $grid;
}

function transform($grid) {
    $newGrid = [];
    foreach ($grid as $i => $row) {
        $newGrid[$i] = [];
        foreach ($row as $j => $acre) {
            $newGrid[$i][$j] = nextAcreState($grid, $i, $j);
        }
    }
    return $newGrid;
}

function nextAcreState($grid, $i, $j) {
    switch ($grid[$i][$j]) {
        case Open:
            if (countAdjacent($grid, $i, $j, Trees) >= 3) {
                return Trees;
            }
            break;
        case Trees:
            if (countAdjacent($grid, $i, $j, Lumberyard) >= 3) {
                return Lumberyard;
            }
            break;
        case Lumberyard:
            if (countAdjacent($grid, $i, $j, Lumberyard) >= 1 && countAdjacent($grid, $i, $j, Trees) >= 1) {
                return Lumberyard;
            }
            return Open;
    }
    return $grid[$i][$j];
}

function countAdjacent($grid, $i, $j, $acreType) {
    $count = 0;
    for ($x = -1; $x <= 1; $x++) {
        for ($y = -1; $y <= 1; $y++) {
            if ($x == 0 && $y == 0) {
                continue;
            }
            if ($i + $x >= 0 && $i + $x < count($grid) && $j + $y >= 0 && $j + $y < count($grid[$i]) && $grid[$i + $x][$j + $y] == $acreType) {
                $count++;
            }
        }
    }
    return $count;
}

function countResources($grid) {
    $wooded = 0;
    $lumberyards = 0;
    foreach ($grid as $row) {
        foreach ($row as $acre) {
            switch ($acre) {
                case Trees:
                    $wooded++;
                    break;
                case Lumberyard:
                    $lumberyards++;
                    break;
            }
        }
    }
    return [$wooded, $lumberyards];
}

function gridToString($grid) {
    $output = "";
    foreach ($grid as $row) {
        $output .= implode("", $row) . PHP_EOL;
    }
    return $output;
}

main();
?>
