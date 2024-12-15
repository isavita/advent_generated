
<?php

function solveMachine(array $m): int {
    $minCost = -1;
    for ($aCount = 0; $aCount <= 100; $aCount++) {
        for ($bCount = 0; $bCount <= 100; $bCount++) {
            $x = $m['ax'] * $aCount + $m['bx'] * $bCount;
            $y = $m['ay'] * $aCount + $m['by'] * $bCount;
            if ($x === $m['px'] && $y === $m['py']) {
                $cost = $aCount * 3 + $bCount;
                if ($minCost < 0 || $cost < $minCost) {
                    $minCost = $cost;
                }
            }
        }
    }
    return $minCost;
}

function parseLine(string $s): array {
    $parts = explode(',', trim($s));
    $xp = trim($parts[0]);
    $yp = trim($parts[1]);
    return [parseVal($xp), parseVal($yp)];
}

function parsePrize(string $s): array {
    $parts = explode(',', trim($s));
    $xp = trim($parts[0]);
    $yp = trim($parts[1]);
    return [parseValPrize($xp), parseValPrize($yp)];
}

function parseVal(string $s): int {
    $s = trim($s);
    $s = ltrim($s, 'X+');
    $s = ltrim($s, 'Y+');
    $s = ltrim($s, 'X=');
    $s = ltrim($s, 'Y=');
    return (int)$s;
}

function parseValPrize(string $s): int {
    $s = trim($s);
    $s = ltrim($s, 'X=');
    $s = ltrim($s, 'Y=');
    return (int)$s;
}

function parseMachine(array $lines): array {
    $m = [];
    foreach ($lines as $l) {
        $l = str_replace(['Button A:', 'Button B:', 'Prize:'], ['A:', 'B:', 'P:'], $l);
        if (str_starts_with($l, 'A:')) {
            [$m['ax'], $m['ay']] = parseLine(substr($l, 2));
        } elseif (str_starts_with($l, 'B:')) {
            [$m['bx'], $m['by']] = parseLine(substr($l, 2));
        } elseif (str_starts_with($l, 'P:')) {
            [$m['px'], $m['py']] = parsePrize(substr($l, 2));
        }
    }
    return $m;
}

function readInput(string $filename): array {
    $machines = [];
    $lines = [];
    $file = fopen($filename, 'r');
    if ($file) {
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
    }
    return $machines;
}

$machines = readInput('input.txt');
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
    echo count($results) . " " . array_sum($results) . "\n";
}
?>
