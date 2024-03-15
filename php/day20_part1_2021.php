<?php
$algorithm = '';
$image = [];

readInput('input.txt', $algorithm, $image);
$image = enhanceImage($image, $algorithm, 2);
echo countLitPixels($image) . PHP_EOL;

function readInput($filename, &$algorithm, &$image) {
    $file = fopen($filename, 'r');
    $algorithm = str_replace("\n", '', fgets($file));
    fgets($file); // Skip the empty line

    while ($line = fgets($file)) {
        $image[] = str_split(rtrim($line));
    }

    fclose($file);
}

function enhanceImage($image, $algorithm, $times) {
    for ($i = 0; $i < $times; $i++) {
        $image = applyAlgorithm($image, $algorithm, $i % 2 == 1 && $algorithm[0] == '#');
    }
    return $image;
}

function applyAlgorithm($image, $algorithm, $flip) {
    $enhancedImage = array_fill(0, count($image) + 2, array_fill(0, count($image[0]) + 2, '.'));
    for ($i = 0; $i < count($enhancedImage); $i++) {
        for ($j = 0; $j < count($enhancedImage[$i]); $j++) {
            $index = calculateIndex($i - 1, $j - 1, $image, $flip);
            $enhancedImage[$i][$j] = $algorithm[$index];
        }
    }
    return $enhancedImage;
}

function calculateIndex($i, $j, $image, $flip) {
    $index = 0;
    for ($di = -1; $di <= 1; $di++) {
        for ($dj = -1; $dj <= 1; $dj++) {
            $index <<= 1;
            if ($i + $di >= 0 && $i + $di < count($image) && $j + $dj >= 0 && $j + $dj < count($image[0])) {
                if ($image[$i + $di][$j + $dj] == '#') {
                    $index |= 1;
                }
            } elseif ($flip) {
                $index |= 1;
            }
        }
    }
    return $index;
}

function countLitPixels($image) {
    $count = 0;
    foreach ($image as $row) {
        foreach ($row as $pixel) {
            if ($pixel == '#') {
                $count++;
            }
        }
    }
    return $count;
}
?>