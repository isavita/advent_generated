
<?php

$input = file_get_contents("input.txt");
$seatingArea = array_map('str_split', explode("\n", $input));

$stabilized = false;
while (!$stabilized) {
    list($seatingArea, $stabilized) = simulateSeating($seatingArea);
}

echo countOccupiedSeats($seatingArea) . PHP_EOL;

function simulateSeating($seatingArea) {
    $rows = count($seatingArea);
    $cols = count($seatingArea[0]);
    $newSeatingArea = $seatingArea;
    $stabilized = true;

    for ($i = 0; $i < $rows; $i++) {
        for ($j = 0; $j < $cols; $j++) {
            switch ($seatingArea[$i][$j]) {
                case 'L':
                    if (countAdjacentOccupied($seatingArea, $i, $j) == 0) {
                        $newSeatingArea[$i][$j] = '#';
                        $stabilized = false;
                    }
                    break;
                case '#':
                    if (countAdjacentOccupied($seatingArea, $i, $j) >= 4) {
                        $newSeatingArea[$i][$j] = 'L';
                        $stabilized = false;
                    }
                    break;
            }
        }
    }

    return array($newSeatingArea, $stabilized);
}

function countAdjacentOccupied($seatingArea, $row, $col) {
    $count = 0;
    $rows = count($seatingArea);
    $cols = count($seatingArea[0]);

    for ($i = $row - 1; $i <= $row + 1; $i++) {
        for ($j = $col - 1; $j <= $col + 1; $j++) {
            if ($i == $row && $j == $col) {
                continue;
            }
            if ($i >= 0 && $i < $rows && $j >= 0 && $j < $cols) {
                if ($seatingArea[$i][$j] == '#') {
                    $count++;
                }
            }
        }
    }

    return $count;
}

function countOccupiedSeats($seatingArea) {
    $count = 0;
    foreach ($seatingArea as $row) {
        foreach ($row as $seat) {
            if ($seat == '#') {
                $count++;
            }
        }
    }
    return $count;
}
?>
