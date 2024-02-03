
<?php

$data = file_get_contents("input.txt");
$imageData = trim($data);

$width = 25;
$height = 6;
$layerSize = $width * $height;

$minZeros = $layerSize + 1;
$result = 0;

for ($i = 0; $i < strlen($imageData); $i += $layerSize) {
    $layer = substr($imageData, $i, min($layerSize, strlen($imageData) - $i));
    $zeroCount = 0;
    $oneCount = 0;
    $twoCount = 0;

    for ($j = 0; $j < strlen($layer); $j++) {
        $pixel = $layer[$j];
        switch ($pixel) {
            case '0':
                $zeroCount++;
                break;
            case '1':
                $oneCount++;
                break;
            case '2':
                $twoCount++;
                break;
        }
    }

    if ($zeroCount < $minZeros) {
        $minZeros = $zeroCount;
        $result = $oneCount * $twoCount;
    }
}

echo $result . "\n";
