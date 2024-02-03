
<?php

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);
$points = [];
$folds = [];
$readingPoints = true;

foreach ($lines as $line) {
    if ($line === "") {
        $readingPoints = false;
        continue;
    }

    if ($readingPoints) {
        $coords = explode(",", $line);
        $x = (int)$coords[0];
        $y = (int)$coords[1];
        $points["$x,$y"] = true;
    } else {
        $folds[] = $line;
    }
}

$fold = explode(" ", $folds[0])[2];
list($axis, $value) = explode("=", $fold);
$newPoints = [];

if ($axis === "x") {
    foreach ($points as $point => $_) {
        list($x, $y) = explode(",", $point);
        if ($x > $value) {
            $x = 2 * $value - $x;
        }
        $newPoints["$x,$y"] = true;
    }
} else {
    foreach ($points as $point => $_) {
        list($x, $y) = explode(",", $point);
        if ($y > $value) {
            $y = 2 * $value - $y;
        }
        $newPoints["$x,$y"] = true;
    }
}

echo count($newPoints) . "\n";
?>
