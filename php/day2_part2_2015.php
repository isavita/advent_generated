
<?php

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);
$totalRibbon = 0;

foreach ($lines as $line) {
    $dimensions = explode("x", $line);
    if (count($dimensions) != 3) {
        die("Invalid input format");
    }

    $l = (int)$dimensions[0];
    $w = (int)$dimensions[1];
    $h = (int)$dimensions[2];

    // Calculate ribbon for the bow
    $bow = $l * $w * $h;

    // Calculate ribbon for wrapping (smallest perimeter)
    $sides = [$l, $w, $h];
    sort($sides);
    $wrap = 2*$sides[0] + 2*$sides[1];

    $totalRibbon += $bow + $wrap;
}

echo $totalRibbon . PHP_EOL;
?>
