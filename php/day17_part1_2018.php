
<?php

$input = trim(file_get_contents("input.txt"));
$lines = preg_split("/[\n]+/", $input);

$ground = [['+']];
$maxX = $minX = $maxY = 0;
$minY = 20;
$xOffset = 500;
$yOffset = 0;

foreach ($lines as $line) {
    preg_match_all("/[=, .]+/", $line, $matches);
    $split = preg_split("/[=, .]+/", $line);

    if ($split[0] == "x") {
        $x = (int)$split[1] - $xOffset;
        $y1 = (int)$split[3] - $yOffset;
        $y2 = (int)$split[4] - $yOffset;

        while ($x >= $maxX) {
            $maxX++;
            foreach ($ground as &$row) {
                $row[] = '.';
            }
        }
        while ($x <= $minX) {
            $minX--;
            foreach ($ground as &$row) {
                array_unshift($row, '.');
            }
        }
        while ($y2 > $maxY) {
            $maxY++;
            $ground[] = array_fill(0, count($ground[0]), '.');
        }
        if ($y1 < $minY) $minY = $y1;
        for ($i = $y1; $i <= $y2; $i++) {
            $ground[$i][$x - $minX] = '#';
        }
    } else {
        $y = (int)$split[1] - $yOffset;
        $x1 = (int)$split[3] - $xOffset;
        $x2 = (int)$split[4] - $xOffset;

        while ($y > $maxY) {
            $maxY++;
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
            foreach ($ground as &$row) {
                array_unshift($row, '.');
            }
        }
        for ($i = $x1; $i <= $x2; $i++) {
            $ground[$y][$i - $minX] = '#';
        }
        if ($y < $minY) $minY = $y;
    }
}

$waterCount = $flowCount = 0;
$roundLimit = 200000;

while ($ground[1][-1 * $minX] != '|' && $waterCount < $roundLimit) {
    $canMove = true;
    $x = -$minX;
    $y = 1;
    $tryLeft = 0;

    while ($canMove) {
        if ($y + 1 > $maxY || $ground[$y + 1][$x] == '|') {
            $ground[$y][$x] = '|';
            $canMove = false;
            if ($y >= $minY) $flowCount++;
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

echo $flowCount + $waterCount;
