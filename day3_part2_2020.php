
<?php

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);

$slopes = [
    [1, 1],
    [3, 1],
    [5, 1],
    [7, 1],
    [1, 2]
];

$product = 1;
foreach ($slopes as $slope) {
    $treeCount = 0;
    $pos = 0;
    for ($i = 0; $i < count($lines); $i += $slope[1]) {
        if ($lines[$i][$pos] == '#') {
            $treeCount++;
        }
        $pos = ($pos + $slope[0]) % strlen($lines[$i]);
    }
    $product *= $treeCount;
}

echo $product . PHP_EOL;
?>
