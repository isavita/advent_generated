
<?php

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);
$parts = explode(", ", $lines[0]);
$xRange = explode("..", substr($parts[0], 15));
$yRange = explode("..", substr($parts[1], 2));
$xMin = (int)$xRange[0];
$xMax = (int)$xRange[1];
$yMin = (int)$yRange[0];
$yMax = (int)$yRange[1];

$maxY = -1 << 30;
for ($xVel = -1000; $xVel <= 1000; $xVel++) {
    for ($yVel = -1000; $yVel <= 1000; $yVel++) {
        $xPos = 0;
        $yPos = 0;
        $curXVel = $xVel;
        $curYVel = $yVel;
        $highestY = $yPos;
        while (true) {
            $xPos += $curXVel;
            $yPos += $curYVel;

            if ($xPos >= $xMin && $xPos <= $xMax && $yPos >= $yMin && $yPos <= $yMax) {
                if ($highestY > $maxY) {
                    $maxY = $highestY;
                }
                break;
            }

            if (isMovingAway($xPos, $yPos, $curXVel, $curYVel, $xMin, $xMax, $yMin, $yMax)) {
                break;
            }

            if ($curXVel > 0) {
                $curXVel--;
            } elseif ($curXVel < 0) {
                $curXVel++;
            }

            $curYVel--;
            if ($yPos > $highestY) {
                $highestY = $yPos;
            }
        }
    }
}

echo $maxY . PHP_EOL;

function isMovingAway($xPos, $yPos, $xVel, $yVel, $xMin, $xMax, $yMin, $yMax) {
    if ($xPos < $xMin && $xVel < 0) {
        return true;
    }
    if ($xPos > $xMax && $xVel > 0) {
        return true;
    }
    if ($yPos < $yMin && $yVel < 0) {
        return true;
    }
    return false;
}
?>
