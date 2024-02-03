
<?php

$seatingArea = [];
$handle = fopen("input.txt", "r");
if ($handle) {
    while (($line = fgets($handle)) !== false) {
        $seatingArea[] = str_split(trim($line));
    }

    fclose($handle);
} else {
    echo "Error opening file\n";
    exit;
}

$directions = [
    [-1, -1], [0, -1], [1, -1],
    [-1, 0], [1, 0],
    [-1, 1], [0, 1], [1, 1],
];

$stabilized = false;
while (!$stabilized) {
    list($seatingArea, $stabilized) = simulateSeatingPartTwo($seatingArea);
}

echo countOccupiedSeats($seatingArea) . "\n";

function simulateSeatingPartTwo($seatingArea) {
    $rows = count($seatingArea);
    $cols = count($seatingArea[0]);
    $newSeatingArea = $seatingArea;
    $stabilized = true;

    for ($i = 0; $i < $rows; $i++) {
        for ($j = 0; $j < $cols; $j++) {
            switch ($seatingArea[$i][$j]) {
                case 'L':
                    if (countVisibleOccupied($seatingArea, $i, $j) == 0) {
                        $newSeatingArea[$i][$j] = '#';
                        $stabilized = false;
                    }
                    break;
                case '#':
                    if (countVisibleOccupied($seatingArea, $i, $j) >= 5) {
                        $newSeatingArea[$i][$j] = 'L';
                        $stabilized = false;
                    }
                    break;
            }
        }
    }

    return [$newSeatingArea, $stabilized];
}

function countVisibleOccupied($seatingArea, $row, $col) {
    $count = 0;
    global $directions;

    foreach ($directions as $dir) {
        list($dx, $dy) = $dir;
        for ($r = $row + $dy, $c = $col + $dx; $r >= 0 && $r < count($seatingArea) && $c >= 0 && $c < count($seatingArea[0]); $r += $dy, $c += $dx) {
            if ($seatingArea[$r][$c] == 'L') {
                break;
            }
            if ($seatingArea[$r][$c] == '#') {
                $count++;
                break;
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
