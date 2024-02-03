
<?php

const screenWidth = 50;
const screenHeight = 6;

$input = file_get_contents("input.txt");
$instructions = explode("\n", $input);

$screen = array_fill(0, screenHeight, array_fill(0, screenWidth, false));

foreach ($instructions as $instruction) {
    processInstruction($instruction, $screen);
}

echo countLitPixels($screen) . PHP_EOL;

function processInstruction($instruction, &$screen) {
    $rectRegex = '/rect (\d+)x(\d+)/';
    $rotateRowRegex = '/rotate row y=(\d+) by (\d+)/';
    $rotateColumnRegex = '/rotate column x=(\d+) by (\d+)/';

    if (preg_match($rectRegex, $instruction, $matches)) {
        $a = (int)$matches[1];
        $b = (int)$matches[2];
        rect($screen, $a, $b);
    } elseif (preg_match($rotateRowRegex, $instruction, $matches)) {
        $a = (int)$matches[1];
        $b = (int)$matches[2];
        rotateRow($screen, $a, $b);
    } elseif (preg_match($rotateColumnRegex, $instruction, $matches)) {
        $a = (int)$matches[1];
        $b = (int)$matches[2];
        rotateColumn($screen, $a, $b);
    }
}

function rect(&$screen, $a, $b) {
    for ($y = 0; $y < $b; $y++) {
        for ($x = 0; $x < $a; $x++) {
            $screen[$y][$x] = true;
        }
    }
}

function rotateRow(&$screen, $row, $shift) {
    $temp = array_fill(0, screenWidth, false);
    foreach ($screen[$row] as $i => $value) {
        $temp[($i + $shift) % screenWidth] = $value;
    }
    $screen[$row] = $temp;
}

function rotateColumn(&$screen, $col, $shift) {
    $temp = array_fill(0, screenHeight, false);
    foreach ($screen as $i => $row) {
        $temp[($i + $shift) % screenHeight] = $row[$col];
    }
    foreach ($screen as $i => $row) {
        $screen[$i][$col] = $temp[$i];
    }
}

function countLitPixels($screen) {
    $count = 0;
    foreach ($screen as $row) {
        foreach ($row as $pixel) {
            if ($pixel) {
                $count++;
            }
        }
    }
    return $count;
}
