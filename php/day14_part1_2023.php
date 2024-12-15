
<?php

function calculateLoad(array $platform): int {
    $rows = count($platform);
    $load = 0;
    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < strlen($platform[$r]); $c++) {
            if ($platform[$r][$c] === 'O') {
                $load += ($rows - $r);
            }
        }
    }
    return $load;
}

function tiltNorth(array $platform): array {
    $rows = count($platform);
    $cols = strlen($platform[0]);
    
    for ($c = 0; $c < $cols; $c++) {
        $nextEmptyRow = 0;
        for ($r = 0; $r < $rows; $r++) {
            if ($platform[$r][$c] === 'O') {
                if ($r !== $nextEmptyRow) {
                    $platform[$nextEmptyRow][$c] = 'O';
                    $platform[$r][$c] = '.';
                }
                $nextEmptyRow++;
            } else if ($platform[$r][$c] === '#') {
                $nextEmptyRow = $r + 1;
            }
        }
    }
    return $platform;
}


$input = file('input.txt', FILE_IGNORE_NEW_LINES);

$tiltedPlatform = tiltNorth($input);
$totalLoad = calculateLoad($tiltedPlatform);

echo $totalLoad . PHP_EOL;

?>
