<?php

function main() {
    $input = trim(file_get_contents("input.txt"));
    $result = solve($input);
    echo $result . "\n";
}

function solve($input) {
    $scanners = parseInput($input);
    
    $settled = [$scanners[0]];
    $settled[0]['absoluteCoords'] = $settled[0]['relativeCoords'];
    fillAbsoluteCoordsMap($settled[0]);

    $undetermined = array_slice($scanners, 1);

    while (count($undetermined) > 0) {
        foreach ($undetermined as $i => $undet) {
            list($maybeUpdated, $ok) = findAbsoluteCoordsForScanner($undet, $settled);
            if ($ok) {
                $settled[] = $maybeUpdated;
                array_splice($undetermined, $i, 1);
                break;
            }
        }
    }

    $furthest = 0;
    foreach ($settled as $i => $s1) {
        foreach ($settled as $j => $s2) {
            if ($i === $j) continue;
            $manhattanDist = abs($s1['x'] - $s2['x']) + abs($s1['y'] - $s2['y']) + abs($s1['z'] - $s2['z']);
            if ($manhattanDist > $furthest) {
                $furthest = $manhattanDist;
            }
        }
    }
    return $furthest;
}

function fillAbsoluteCoordsMap(&$scanner) {
    $scanner['absoluteCoordsMap'] = [];
    if (empty($scanner['absoluteCoords'])) {
        throw new Exception("absolute coords not set for scanner " . $scanner['number']);
    }
    foreach ($scanner['absoluteCoords'] as $ac) {
        $scanner['absoluteCoordsMap'][$ac[0] . ',' . $ac[1] . ',' . $ac[2]] = true;
    }
}

function findAbsoluteCoordsForScanner($undet, $settled) {
    foreach ($undet['rotations'] as $rotatedCoords) {
        foreach ($settled as $set) {
            foreach ($set['absoluteCoords'] as $absCoord) {
                foreach ($rotatedCoords as $relativeCoord) {
                    $unsettledAbsoluteCoords = makeAbsoluteCoordsList($absCoord, $relativeCoord, $rotatedCoords);

                    $matchingCount = 0;
                    foreach ($unsettledAbsoluteCoords as $ac) {
                        if (isset($set['absoluteCoordsMap'][$ac[0] . ',' . $ac[1] . ',' . $ac[2]])) {
                            $matchingCount++;
                        }
                    }

                    if ($matchingCount >= 12) {
                        $undet['relativeCoords'] = $rotatedCoords;
                        $undet['absoluteCoords'] = $unsettledAbsoluteCoords;
                        fillAbsoluteCoordsMap($undet);
                        $undet['x'] = $absCoord[0] - $relativeCoord[0];
                        $undet['y'] = $absCoord[1] - $relativeCoord[1];
                        $undet['z'] = $absCoord[2] - $relativeCoord[2];
                        return [$undet, true];
                    }
                }
            }
        }
    }
    return [$undet, false];
}

function makeAbsoluteCoordsList($absolute, $relative, $relativeCoords) {
    $diff = [
        $absolute[0] - $relative[0],
        $absolute[1] - $relative[1],
        $absolute[2] - $relative[2]
    ];

    $absCoords = [];
    foreach ($relativeCoords as $c) {
        $absCoords[] = [
            $diff[0] + $c[0],
            $diff[1] + $c[1],
            $diff[2] + $c[2]
        ];
    }

    return $absCoords;
}

function parseInput($input) {
    $ans = [];
    $rawScanners = explode("\n\n", $input);
    foreach ($rawScanners as $rawScanner) {
        $lines = explode("\n", $rawScanner);
        sscanf($lines[0], "--- scanner %d ---", $number);

        $coords = [];
        for ($i = 1; $i < count($lines); $i++) {
            sscanf($lines[$i], "%d,%d,%d", $x, $y, $z);
            $coords[] = [$x, $y, $z];
        }

        $sc = [
            'number' => $number,
            'x' => 0,
            'y' => 0,
            'z' => 0,
            'relativeCoords' => $coords,
            'absoluteCoords' => [],
            'absoluteCoordsMap' => []
        ];
        fillRotations($sc);
        $ans[] = $sc;
    }

    return $ans;
}

function fillRotations(&$scanner) {
    $posX = $scanner['relativeCoords'];
    $dir2 = $dir3 = $dir4 = $dir5 = $dir6 = [];

    foreach ($posX as $c) {
        list($x, $y, $z) = $c;
        $dir2[] = [$x, -$y, -$z];
        $dir3[] = [$x, -$z, $y];
        $dir4[] = [-$y, -$z, $x];
        $dir5[] = [-$x, -$z, -$y];
        $dir6[] = [$y, -$z, -$x];
    }
    $sixRotations = [$posX, $dir2, $dir3, $dir4, $dir5, $dir6];

    $finalRotations = [];
    foreach ($sixRotations as $rotation) {
        $r2 = $r3 = $r4 = [];
        foreach ($rotation as $c) {
            list($x, $y, $z) = $c;
            $r2[] = [-$y, $x, $z];
            $r3[] = [-$x, -$y, $z];
            $r4[] = [$y, -$x, $z];
        }
        $finalRotations = array_merge($finalRotations, [$rotation, $r2, $r3, $r4]);
    }
    $scanner['rotations'] = $finalRotations;
}

main();

?>