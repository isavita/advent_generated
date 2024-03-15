<?php

function strToInt($s) {
    return intval($s);
}

function regSplit($text, $delimeter) {
    return preg_split("/$delimeter/", $text);
}

$input = file_get_contents("input.txt");
$inputStr = trim($input);
$lines = regSplit($inputStr, "\n");

$ground = [['#']];
$maxX = $minX = 0;
$maxY = 20;
$minY = 0;
$xOffset = 500;
$yOffset = 0;

foreach ($lines as $line) {
    $split = regSplit($line, "[=, .]+");
    if ($split[0] == "x") {
        $x = strToInt($split[1]) - $xOffset;
        $y1 = strToInt($split[3]) - $yOffset;
        $y2 = strToInt($split[4]) - $yOffset;

        while ($x >= $maxX) {
            $maxX++;
            foreach ($ground as &$row) {
                $row[] = '.';
            }
        }
        while ($x <= $minX) {
            $minX--;
            array_unshift($ground[0], '.');
            foreach ($ground as &$row) {
                array_unshift($row, '.');
            }
        }
        while ($y2 >= count($ground)) {
            $ground[] = array_fill(0, count($ground[0]), '.');
        }
        if ($y1 < $minY) {
            $minY = $y1;
        }
        for ($i = $y1; $i <= $y2; $i++) {
            $ground[$i][$x - $minX] = '#';
        }
    } else {
        $y = strToInt($split[1]) - $yOffset;
        $x1 = strToInt($split[3]) - $xOffset;
        $x2 = strToInt($split[4]) - $xOffset;

        while ($y >= count($ground)) {
            $ground[] = array_fill(0, count($ground[0]), '.');
        }
        while ($x2 >= $maxX) {
            $maxX++;
            foreach ($ground as &$row) {
                $row[] = '.';
            }
        }
        while ($x1 <= $minX) {
            $minX--;
            array_unshift($ground[0], '.');
            foreach ($ground as &$row) {
                array_unshift($row, '.');
            }
        }
        for ($i = $x1; $i <= $x2; $i++) {
            $ground[$y][$i - $minX] = '#';
        }
        if ($y < $minY) {
            $minY = $y;
        }
    }
}

$waterCount = 0;
$flowCount = 0;
$roundLimit = 200000;

while ($ground[1][-$minX] != '|' && $waterCount < $roundLimit) {
    $canMove = true;
    $x = -$minX;
    $y = 1;
    $tryLeft = 0;
    while ($canMove) {
        if ($y + 1 >= count($ground) || $ground[$y + 1][$x] == '|') {
            $ground[$y][$x] = '|';
            $canMove = false;
            if ($y >= $minY) {
                $flowCount++;
            }
        } elseif ($ground[$y + 1][$x] == '.') {
            $y++;
            $tryLeft = 0;
        } elseif ($ground[$y + 1][$x] == '#' || $ground[$y + 1][$x] == '~') {
            if (($tryLeft == 1 && $ground[$y][$x - 1] == '|') ||
                ($tryLeft == 2 && $ground[$y][$x + 1] == '|') ||
                ($ground[$y][$x + 1] == '|' && $ground[$y][$x - 1] != '.') ||
                ($ground[$y][$x + 1] != '.' && $ground[$y][$x - 1] == '|')) {
                $ground[$y][$x] = '|';
                $flowCount++;
                $canMove = false;
                for ($i = $x + 1; $ground[$y][$i] == '~'; $i++) {
                    $ground[$y][$i] = '|';
                    $waterCount--;
                    $flowCount++;
                }
                for ($i = $x - 1; $ground[$y][$i] == '~'; $i--) {
                    $ground[$y][$i] = '|';
                    $waterCount--;
                    $flowCount++;
                }
            } elseif (($tryLeft == 0 && $ground[$y][$x - 1] == '.') ||
                     ($tryLeft == 1 && $ground[$y][$x - 1] == '.')) {
                $x--;
                $tryLeft = 1;
            } elseif (($tryLeft == 0 && $ground[$y][$x + 1] == '.') ||
                     ($tryLeft == 2 && $ground[$y][$x + 1] == '.')) {
                $x++;
                $tryLeft = 2;
            } else {
                $canMove = false;
                $ground[$y][$x] = '~';
                $waterCount++;
            }
        }
    }
}

echo $waterCount . "\n";