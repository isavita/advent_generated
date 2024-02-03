
<?php

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);
$discs = [];

foreach ($lines as $line) {
    preg_match('/Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)./', $line, $matches);
    $totalPositions = (int)$matches[2];
    $startPosition = (int)$matches[3];
    $discs[] = ['totalPositions' => $totalPositions, 'startPosition' => $startPosition];
}

// Add the new disc as per Part Two's requirement
$discs[] = ['totalPositions' => 11, 'startPosition' => 0];

$time = 0;
while (true) {
    if (checkDiscs($discs, $time)) {
        echo $time . PHP_EOL;
        break;
    }
    $time++;
}

function checkDiscs($discs, $time) {
    foreach ($discs as $i => $disc) {
        $position = ($disc['startPosition'] + $time + $i + 1) % $disc['totalPositions'];
        if ($position !== 0) {
            return false;
        }
    }
    return true;
}
?>
