
<?php

$totalRows = 400000; // Total number of rows to generate

$firstRow = trim(file_get_contents("input.txt"));
$safeTilesCount = countSafeTiles($firstRow, $totalRows);
echo $safeTilesCount . PHP_EOL;

function countSafeTiles($firstRow, $totalRows) {
    $currentRow = $firstRow;
    $safeCount = substr_count($currentRow, '.');

    for ($i = 1; $i < $totalRows; $i++) {
        $nextRow = '';
        for ($j = 0; $j < strlen($currentRow); $j++) {
            if (isTrap($j-1, $j, $j+1, $currentRow)) {
                $nextRow .= '^';
            } else {
                $nextRow .= '.';
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
