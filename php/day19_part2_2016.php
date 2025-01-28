
<?php

/**
 * Reads the number of elves from input.txt and calculates the winning elf
 * for both parts of the puzzle.
 *
 * @return void
 */
function solvePuzzle(): void
{
    $elvesCount = (int) file_get_contents("input.txt");

    echo "Part 1: " . solvePart1($elvesCount) . "\n";
    echo "Part 2: " . solvePart2($elvesCount) . "\n";
}

/**
 * Solves Part 1 of the puzzle using a Josephus problem variation.
 *
 * @param int $n The number of elves.
 * @return int The winning elf's position.
 */
function solvePart1(int $n): int
{
    $highestPowerOf2 = 1;
    while ($highestPowerOf2 * 2 <= $n) {
        $highestPowerOf2 *= 2;
    }
    return 2 * ($n - $highestPowerOf2) + 1;
}

/**
 * Solves Part 2 of the puzzle by simulating the present stealing process.
 *
 * @param int $n The number of elves.
 * @return int The winning elf's position.
 */
function solvePart2(int $n): int
{
    if ($n == 1) {
        return 1;
    }
    
    $powerOf3 = 1;
    while ($powerOf3 * 3 < $n) {
        $powerOf3 *= 3;
    }

    if ($n == $powerOf3) {
        return $n;
    }

    if ($n - $powerOf3 <= $powerOf3) {
        return $n - $powerOf3;
    } else {
        return $n - $powerOf3 + ($n - 2 * $powerOf3);
    }
}

solvePuzzle();
