
<?php

function readInput(string $filename): array {
    $machines = [];
    $lines = [];
    $file = fopen($filename, 'r');
    if (!$file) {
        throw new Exception("Could not open file: $filename");
    }
    while (($line = fgets($file)) !== false) {
        $line = trim($line);
        if ($line === '') {
            if (!empty($lines)) {
                $machines[] = parseMachine($lines);
                $lines = [];
            }
        } else {
            $lines[] = $line;
        }
    }
    if (!empty($lines)) {
        $machines[] = parseMachine($lines);
    }
    fclose($file);
    return $machines;
}

function parseMachine(array $lines): array {
    $machine = ['ax' => 0, 'ay' => 0, 'bx' => 0, 'by' => 0, 'px' => 0, 'py' => 0];
    foreach ($lines as $line) {
        $line = str_replace(['Button A:', 'Button B:', 'Prize:'], ['A:', 'B:', 'P:'], $line);
        if (strpos($line, 'A:') === 0) {
            list($machine['ax'], $machine['ay']) = parseLine(substr($line, 2));
        } elseif (strpos($line, 'B:') === 0) {
            list($machine['bx'], $machine['by']) = parseLine(substr($line, 2));
        } elseif (strpos($line, 'P:') === 0) {
            list($machine['px'], $machine['py']) = parsePrize(substr($line, 2));
        }
    }
    return $machine;
}

function parseLine(string $s): array {
    $parts = explode(',', trim($s));
    return [parseVal($parts[0]), parseVal($parts[1])];
}

function parsePrize(string $s): array {
    $parts = explode(',', trim($s));
    return [parseValPrize($parts[0]), parseValPrize($parts[1])];
}

function parseVal(string $s): int {
    $s = trim($s);
    $s = ltrim($s, 'X+');
    $s = ltrim($s, 'Y+');
    $s = ltrim($s, 'X=');
    $s = ltrim($s, 'Y=');
    return (int) $s;
}

function parseValPrize(string $s): int {
    $s = trim($s);
    $s = ltrim($s, 'X=');
    $s = ltrim($s, 'Y=');
    return (int) $s;
}

function solveMachine(array $m): int {
    $D = $m['ax'] * $m['by'] - $m['ay'] * $m['bx'];
    if ($D === 0) {
        return -1;
    }
    $numA = $m['px'] * $m['by'] - $m['py'] * $m['bx'];
    $numB = -$m['px'] * $m['ay'] + $m['py'] * $m['ax'];
    if ($numA % $D !== 0 || $numB % $D !== 0) {
        return -1;
    }
    $a = $numA / $D;
    $b = $numB / $D;
    if ($a < 0 || $b < 0) {
        return -1;
    }
    return 3 * $a + $b;
}

$offset = 10000000000000;
$machines = readInput('input.txt');
foreach ($machines as &$m) {
    $m['px'] += $offset;
    $m['py'] += $offset;
}
unset($m);

$results = [];
foreach ($machines as $m) {
    $cost = solveMachine($m);
    if ($cost >= 0) {
        $results[] = $cost;
    }
}

if (empty($results)) {
    echo "0 0\n";
} else {
    $count = count($results);
    $sum = array_sum($results);
    echo "$count $sum\n";
}
?>
