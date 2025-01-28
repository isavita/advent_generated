
<?php

function solve() {
    $robots = [];
    $file = fopen('input.txt', 'r');
    if ($file) {
        while (($line = fgets($file)) !== false) {
            preg_match('/p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)/', $line, $matches);
            if ($matches) {
                $robots[] = [
                    'px' => intval($matches[1]),
                    'py' => intval($matches[2]),
                    'vx' => intval($matches[3]),
                    'vy' => intval($matches[4]),
                ];
            }
        }
        fclose($file);
    } else {
        die("Unable to open input.txt\n");
    }

    $width = 101;
    $height = 103;
    $time = 100;

    $grid = array_fill(0, $height, array_fill(0, $width, 0));

    foreach ($robots as &$robot) {
        $final_x = $robot['px'] + $robot['vx'] * $time;
        $final_y = $robot['py'] + $robot['vy'] * $time;

        // Wrap around logic
        $final_x = (($final_x % $width) + $width) % $width;
        $final_y = (($final_y % $height) + $height) % $height;

        $grid[$final_y][$final_x]++;
    }

    $q1 = 0; // Top-left
    $q2 = 0; // Top-right
    $q3 = 0; // Bottom-left
    $q4 = 0; // Bottom-right

    $mid_x = floor($width / 2);
    $mid_y = floor($height / 2);

    for ($y = 0; $y < $height; $y++) {
        for ($x = 0; $x < $width; $x++) {
            if ($grid[$y][$x] > 0) {
                if ($x < $mid_x && $y < $mid_y) {
                    $q1 += $grid[$y][$x];
                } elseif ($x > $mid_x && $y < $mid_y) {
                    $q2 += $grid[$y][$x];
                } elseif ($x < $mid_x && $y > $mid_y) {
                    $q3 += $grid[$y][$x];
                } elseif ($x > $mid_x && $y > $mid_y) {
                    $q4 += $grid[$y][$x];
                }
            }
        }
    }

    echo $q1 * $q2 * $q3 * $q4 . "\n";
}

solve();

?>
