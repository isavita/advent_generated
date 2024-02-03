
<?php

function readFileToMatrix($filePath) {
    $file = fopen($filePath, "r");
    $matrix = [];
    while (!feof($file)) {
        $matrix[] = str_split(trim(fgets($file)));
    }
    fclose($file);
    return $matrix;
}

function sumOfPartNumbers($matrix) {
    $sum = 0;
    $visited = array_fill(0, count($matrix), array_fill(0, count($matrix[0]), false));

    for ($y = 0; $y < count($matrix); $y++) {
        for ($x = 0; $x < count($matrix[$y]); $x++) {
            if (!$visited[$y][$x] && is_numeric($matrix[$y][$x])) {
                list($number, $length) = extractNumber($matrix, $x, $y);
                if (isAdjacentToSymbol($matrix, $x, $y, $length)) {
                    $sum += $number;
                }
                for ($i = 0; $i < $length; $i++) {
                    $visited[$y][$x + $i] = true;
                }
            }
        }
    }
    return $sum;
}

function extractNumber($matrix, $x, $y) {
    $numberStr = "";
    while ($x < count($matrix[$y]) && is_numeric($matrix[$y][$x])) {
        $numberStr .= $matrix[$y][$x];
        $x++;
    }
    $number = (int)$numberStr;
    return array($number, strlen($numberStr));
}

function isAdjacentToSymbol($matrix, $x, $y, $length) {
    for ($i = 0; $i < $length; $i++) {
        if (checkAdjacent($matrix, $x + $i, $y)) {
            return true;
        }
    }
    return false;
}

function checkAdjacent($matrix, $x, $y) {
    for ($dy = -1; $dy <= 1; $dy++) {
        for ($dx = -1; $dx <= 1; $dx++) {
            $adjX = $x + $dx;
            $adjY = $y + $dy;
            if ($adjY >= 0 && $adjY < count($matrix) && $adjX >= 0 && $adjX < count($matrix[$adjY])) {
                if (!is_numeric($matrix[$adjY][$adjX]) && $matrix[$adjY][$adjX] != '.') {
                    return true;
                }
            }
        }
    }
    return false;
}

$matrix = readFileToMatrix("input.txt");
$sum = sumOfPartNumbers($matrix);
echo $sum . "\n";

?>
