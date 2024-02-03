
<?php

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);
$trees = countTrees($lines, 3, 1);
echo $trees . PHP_EOL;

function countTrees($forest, $right, $down) {
    $trees = 0;
    $x = 0;
    $width = strlen($forest[0]);

    for ($y = 0; $y < count($forest); $y += $down) {
        if ($forest[$y][$x % $width] == '#') {
            $trees++;
        }
        $x += $right;
    }

    return $trees;
}
?>
