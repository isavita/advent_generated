
<?php

$iterations = 50;
$expandBy = 1;

function readInput($filename) {
    $file = fopen($filename, "r");
    $algorithm = trim(fgets($file));
    fgets($file); // skip the empty line
    $image = [];
    while ($line = trim(fgets($file))) {
        $row = [];
        for ($i = 0; $i < strlen($line); $i++) {
            $row[] = $line[$i] == '#';
        }
        $image[] = $row;
    }
    fclose($file);
    return [$algorithm, $image];
}

function enhanceImage($algorithm, $image, $useInfiniteLit) {
    $newImage = [];
    for ($i = 0; $i < count($image) + ($GLOBALS['expandBy'] * 2); $i++) {
        $newImage[] = array_fill(0, count($image[0]) + ($GLOBALS['expandBy'] * 2), false);
    }

    for ($y = -$GLOBALS['expandBy']; $y < count($image) + $GLOBALS['expandBy']; $y++) {
        for ($x = -$GLOBALS['expandBy']; $x < count($image[0]) + $GLOBALS['expandBy']; $x++) {
            $index = 0;
            for ($dy = -1; $dy <= 1; $dy++) {
                for ($dx = -1; $dx <= 1; $dx++) {
                    $index <<= 1;
                    $ny = $y + $dy;
                    $nx = $x + $dx;
                    if ($ny >= 0 && $ny < count($image) && $nx >= 0 && $nx < count($image[0])) {
                        if ($image[$ny][$nx]) {
                            $index |= 1;
                        }
                    } elseif ($useInfiniteLit) {
                        $index |= 1;
                    }
                }
            }
            $newImage[$y + $GLOBALS['expandBy']][$x + $GLOBALS['expandBy']] = $algorithm[$index] == '#';
        }
    }
    return $newImage;
}

function countLitPixels($image) {
    $count = 0;
    foreach ($image as $row) {
        foreach ($row as $pixel) {
            if ($pixel) {
                $count++;
            }
        }
    }
    return $count;
}

list($algorithm, $image) = readInput("input.txt");
for ($i = 0; $i < $iterations; $i++) {
    $image = enhanceImage($algorithm, $image, $i % 2 == 1 && $algorithm[0] == '#');
}
echo countLitPixels($image) . PHP_EOL;

?>
