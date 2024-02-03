
<?php

$file = fopen("input.txt", "r");
if ($file === false) {
    echo "Error opening file\n";
    exit(1);
}

$line = fgets($file);
$parts = explode(", ", $line);
$xRange = explode("..", substr($parts[0], 15));
$yRange = explode("..", substr($parts[1], 2));
$xMin = intval($xRange[0]);
$xMax = intval($xRange[1]);
$yMin = intval($yRange[0]);
$yMax = intval($yRange[1]);

$velocities = [];
for ($xVel = -1000; $xVel <= 1000; $xVel++) {
    for ($yVel = -1000; $yVel <= 1000; $yVel++) {
        $xPos = 0;
        $yPos = 0;
        $curXVel = $xVel;
        $curYVel = $yVel;
        $inTargetArea = false;
        while (true) {
            $xPos += $curXVel;
            $yPos += $curYVel;

            if ($xPos >= $xMin && $xPos <= $xMax && $yPos >= $yMin && $yPos <= $yMax) {
                $inTargetArea = true;
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
        }

        if ($inTargetArea) {
            $velocityKey = $xVel . "," . $yVel;
            $velocities[$velocityKey] = true;
        }
    }
}

echo count($velocities) . "\n";

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
