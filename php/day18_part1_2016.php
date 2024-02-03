
<?php

$filename = "input.txt";
$firstRow = readFirstRow($filename);
$totalRows = 40;
$safeTilesCount = countSafeTiles($firstRow, $totalRows);
echo $safeTilesCount . PHP_EOL;

function readFirstRow($filename) {
    $file = fopen($filename, "r");
    if ($file) {
        $firstRow = fgets($file);
        fclose($file);
        return $firstRow;
    } else {
        die("Failed to read the first row");
    }
}

function countSafeTiles($firstRow, $totalRows) {
    $currentRow = $firstRow;
    $safeCount = substr_count($currentRow, '.');

    for ($i = 1; $i < $totalRows; $i++) {
        $nextRow = "";
        for ($j = 0; $j < strlen($currentRow); $j++) {
            if (isTrap($j-1, $j, $j+1, $currentRow)) {
                $nextRow .= "^";
            } else {
                $nextRow .= ".";
                $safeCount++;
            }
        }
        $currentRow = $nextRow;
    }
    return $safeCount;
}

function isTrap($left, $center, $right, $row) {
    $l = safeIfOutOfBounds($left, $row);
    $c = $row[$center];
    $r = safeIfOutOfBounds($right, $row);

    return ($l == '^' && $c == '^' && $r == '.') ||
           ($c == '^' && $r == '^' && $l == '.') ||
           ($l == '^' && $c == '.' && $r == '.') ||
           ($r == '^' && $c == '.' && $l == '.');
}

function safeIfOutOfBounds($index, $row) {
    if ($index < 0 || $index >= strlen($row)) {
        return '.';
    }
    return $row[$index];
}
