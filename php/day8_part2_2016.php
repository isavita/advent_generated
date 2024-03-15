<?php

const SCREEN_WIDTH = 50;
const SCREEN_HEIGHT = 6;

function main(): void {
    $screen = array_fill(0, SCREEN_HEIGHT, array_fill(0, SCREEN_WIDTH, false));
    $instructions = file("input.txt", FILE_IGNORE_NEW_LINES);

    foreach ($instructions as $instruction) {
        processInstruction($instruction, $screen);
    }

    displayScreen($screen);
    echo "Number of lit pixels: " . countLitPixels($screen) . "\n";
}

function processInstruction(string $instruction, array &$screen): void {
    if (preg_match('/rect (\d+)x(\d+)/', $instruction, $matches)) {
        $a = (int)$matches[1];
        $b = (int)$matches[2];
        rect($screen, $a, $b);
    } elseif (preg_match('/rotate row y=(\d+) by (\d+)/', $instruction, $matches)) {
        $row = (int)$matches[1];
        $shift = (int)$matches[2];
        rotateRow($screen, $row, $shift);
    } elseif (preg_match('/rotate column x=(\d+) by (\d+)/', $instruction, $matches)) {
        $col = (int)$matches[1];
        $shift = (int)$matches[2];
        rotateColumn($screen, $col, $shift);
    }
}

function rect(array &$screen, int $a, int $b): void {
    for ($y = 0; $y < $b; $y++) {
        for ($x = 0; $x < $a; $x++) {
            $screen[$y][$x] = true;
        }
    }
}

function rotateRow(array &$screen, int $row, int $shift): void {
    $temp = array_fill(0, SCREEN_WIDTH, false);
    for ($i = 0; $i < SCREEN_WIDTH; $i++) {
        $temp[($i + $shift) % SCREEN_WIDTH] = $screen[$row][$i];
    }
    $screen[$row] = $temp;
}

function rotateColumn(array &$screen, int $col, int $shift): void {
    $temp = array_fill(0, SCREEN_HEIGHT, false);
    for ($i = 0; $i < SCREEN_HEIGHT; $i++) {
        $temp[($i + $shift) % SCREEN_HEIGHT] = $screen[$i][$col];
    }
    for ($i = 0; $i < SCREEN_HEIGHT; $i++) {
        $screen[$i][$col] = $temp[$i];
    }
}

function countLitPixels(array $screen): int {
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

function displayScreen(array $screen): void {
    foreach ($screen as $row) {
        foreach ($row as $pixel) {
            echo $pixel ? '#' : '.';
        }
        echo "\n";
    }
}

main();