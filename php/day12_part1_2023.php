<?php

class Row {
    public $springs;
    public $group;

    public function __construct($springs, $group) {
        $this->springs = $springs;
        $this->group = $group;
    }
}

function parseInput($input) {
    $rows = [];
    foreach ($input as $line) {
        $parts = explode(" ", $line);
        $springs = $parts[0];
        $ints = array_map('intval', explode(",", $parts[1]));
        $row = new Row($springs, $ints);
        $rows[] = $row;
    }
    return $rows;
}

function countArrangementsRecursive($row, $iSprings, $iGroup, $iContiguousDamaged, &$cache) {
    if ($iSprings == strlen($row->springs)) {
        if ($iGroup == count($row->group) && $iContiguousDamaged == 0) {
            return 1;
        } elseif ($iGroup == count($row->group) - 1 && $iContiguousDamaged == $row->group[$iGroup]) {
            return 1;
        }
        return 0;
    }

    $cacheKey = $iSprings . ',' . $iGroup . ',' . $iContiguousDamaged;
    if (isset($cache[$cacheKey])) {
        return $cache[$cacheKey];
    }

    $res = 0;
    $char = $row->springs[$iSprings];
    if ($char == '.' || $char == '?') {
        if ($iContiguousDamaged == 0) {
            $res += countArrangementsRecursive($row, $iSprings + 1, $iGroup, $iContiguousDamaged, $cache);
        } elseif ($iContiguousDamaged == $row->group[$iGroup]) {
            $res += countArrangementsRecursive($row, $iSprings + 1, $iGroup + 1, 0, $cache);
        }
    }
    if ($char == '#' || $char == '?') {
        if ($iGroup < count($row->group) && $iContiguousDamaged < $row->group[$iGroup]) {
            $res += countArrangementsRecursive($row, $iSprings + 1, $iGroup, $iContiguousDamaged + 1, $cache);
        }
    }

    $cache[$cacheKey] = $res;
    return $res;
}

function countArrangements($row) {
    $cache = [];
    return countArrangementsRecursive($row, 0, 0, 0, $cache);
}

function solve($input) {
    $rows = parseInput($input);

    $res = 0;
    foreach ($rows as $row) {
        $res += countArrangements($row);
    }

    return $res;
}

function readFileContents($fileName) {
    $fileContents = file_get_contents($fileName);
    return explode("\n", trim($fileContents));
}

$input = readFileContents("input.txt");
echo solve($input);

?>