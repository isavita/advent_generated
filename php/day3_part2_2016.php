<?php

function isValidTriangle($a, $b, $c) {
    return $a + $b > $c && $a + $c > $b && $b + $c > $a;
}

$fileContent = file_get_contents("input.txt");
$lines = explode("\n", trim($fileContent));
$numbers = [];

foreach ($lines as $line) {
    $row = array_map('intval', preg_split('/\s+/', trim($line)));
    $numbers[] = $row;
}

$validTriangles = 0;
$columnCount = count($numbers[0]);

for ($i = 0; $i < $columnCount; $i++) {
    for ($j = 0; $j < count($numbers); $j += 3) {
        if ($j + 2 < count($numbers)) {
            if (isValidTriangle($numbers[$j][$i], $numbers[$j+1][$i], $numbers[$j+2][$i])) {
                $validTriangles++;
            }
        }
    }
}

echo $validTriangles;

?>