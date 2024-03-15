<?php

function part1($input) {
    $cubes = parseInput($input);

    $onCoords = [];

    foreach ($cubes as $c) {
        if (part1OutOfBounds($c['x1'], $c['x2'], $c['y1'], $c['y2'], $c['z1'], $c['z2'])) {
            continue;
        }

        for ($x = $c['x1']; $x <= $c['x2']; $x++) {
            for ($y = $c['y1']; $y <= $c['y2']; $y++) {
                for ($z = $c['z1']; $z <= $c['z2']; $z++) {
                    $coord = [$x, $y, $z];
                    $onCoords[implode(',', $coord)] = $c['isOn'];
                }
            }
        }
    }

    $count = 0;
    foreach ($onCoords as $b) {
        if ($b) {
            $count++;
        }
    }

    return $count;
}

function part1OutOfBounds($x1, $x2, $y1, $y2, $z1, $z2) {
    if ($x1 < -50 || $x2 > 50 || $y1 < -50 || $y2 > 50 || $z1 < -50 || $z2 > 50) {
        return true;
    }
    return false;
}

function solve($input) {
    $cubes = parseInput($input);

    $finalList = [];
    foreach ($cubes as $c) {
        $toAdd = [];

        foreach ($finalList as $finalCube) {
            $intersection = getIntersection($finalCube, $c);
            if ($intersection !== false) {
                $toAdd[] = $intersection;
            }
        }

        if ($c['isOn']) {
            $toAdd[] = $c;
        }

        $finalList = array_merge($finalList, $toAdd);
    }

    $total = 0;
    foreach ($finalList as $c) {
        $total += volume($c);
    }

    return $total;
}

function getIntersection($c, $c2) {
    $x1 = max($c['x1'], $c2['x1']);
    $x2 = min($c['x2'], $c2['x2']);
    $y1 = max($c['y1'], $c2['y1']);
    $y2 = min($c['y2'], $c2['y2']);
    $z1 = max($c['z1'], $c2['z1']);
    $z2 = min($c['z2'], $c2['z2']);

    if ($x1 > $x2 || $y1 > $y2 || $z1 > $z2) {
        return false;
    }

    $intersectionState = false;
    if ($c['isOn'] && $c2['isOn']) {
        $intersectionState = false;
    } elseif (!$c['isOn'] && !$c2['isOn']) {
        $intersectionState = true;
    } else {
        $intersectionState = $c2['isOn'];
    }

    return [
        'isOn' => $intersectionState,
        'x1' => $x1,
        'x2' => $x2,
        'y1' => $y1,
        'y2' => $y2,
        'z1' => $z1,
        'z2' => $z2
    ];
}

function volume($c) {
    $vol = ($c['x2'] - $c['x1'] + 1) * ($c['y2'] - $c['y1'] + 1) * ($c['z2'] - $c['z1'] + 1);
    return $c['isOn'] ? $vol : -$vol;
}

function parseInput($input) {
    $ans = [];
    foreach (explode("\n", $input) as $line) {
        $parts = explode(" ", $line);

        $x1 = $x2 = $y1 = $y2 = $z1 = $z2 = 0;
        if (sscanf($parts[1], "x=%d..%d,y=%d..%d,z=%d..%d", $x1, $x2, $y1, $y2, $z1, $z2) !== 6) {
            throw new Exception("Failed to parse input line: " . $line);
        }

        if ($x1 > $x2 || $y1 > $y2 || $z1 > $z2) {
            throw new Exception("didn't expect input to have backwards coords, sort them...");
        }

        $ans[] = [
            'isOn' => $parts[0] == "on",
            'x1' => $x1,
            'x2' => $x2,
            'y1' => $y1,
            'y2' => $y2,
            'z1' => $z1,
            'z2' => $z2
        ];
    }
    return $ans;
}

function maxInt($a, $b) {
    return $a > $b ? $a : $b;
}

function minInt($a, $b) {
    return -maxInt(-$a, -$b);
}

$input = file_get_contents('input.txt');
$result = solve($input);
echo $result . "\n";