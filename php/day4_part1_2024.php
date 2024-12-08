
<?php

function countXMAS($grid) {
    $rows = count($grid);
    $cols = count($grid[0]);
    $count = 0;
    $target = "XMAS";
    $targetLen = strlen($target);

    for ($i = 0; $i < $rows; $i++) {
        for ($j = 0; $j < $cols; $j++) {
            // Check all 8 directions
            $directions = [
                [0, 1], [1, 1], [1, 0], [1, -1],
                [0, -1], [-1, -1], [-1, 0], [-1, 1]
            ];

            foreach ($directions as $dir) {
                $x = $i;
                $y = $j;
                $found = true;
                for ($k = 0; $k < $targetLen; $k++) {
                    if (!isset($grid[$x][$y]) || $grid[$x][$y] != $target[$k]) {
                        $found = false;
                        break;
                    }
                    $x += $dir[0];
                    $y += $dir[1];
                }
                if ($found) {
                    $count++;
                }
            }
        }
    }
    return $count;
}


// Read input from file
$input = file('input.txt', FILE_IGNORE_NEW_LINES);

//Clean and format the input.  Assumes a rectangular grid.
$grid = [];
foreach($input as $line){
    $grid[] = str_split($line);
}


$xmasCount = countXMAS($grid);
echo "XMAS appears " . $xmasCount . " times.\n";

?>
