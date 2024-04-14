<?php

function isValidTriangle($a, $b, $c) {
    return $a + $b > $c && $a + $c > $b && $b + $c > $a;
}

$filename = "input.txt";
$file = fopen($filename, "r");

if (!$file) {
    die("Failed to open file");
}

$validTriangles = 0;

while (($line = fgets($file)) !== false) {
    $sides = array_map('intval', preg_split('/\s+/', trim($line)));
    
    if (count($sides) !== 3) {
        echo "Invalid input format\n";
        continue;
    }
    
    if (isValidTriangle($sides[0], $sides[1], $sides[2])) {
        $validTriangles++;
    }
}

fclose($file);

echo $validTriangles;
?>