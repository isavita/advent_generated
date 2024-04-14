<?php

class Scanner {
    public $number;
    public $x = 0;
    public $y = 0;
    public $z = 0;
    public $relativeCoords = [];
    public $rotations = [];
    public $absoluteCoords = [];
    public $absoluteCoordsMap = [];

    public function fillAbsoluteCoordsMap() {
        if (empty($this->absoluteCoords)) {
            throw new Exception("absolute coords not set for scanner {$this->number}");
        }
        foreach ($this->absoluteCoords as $ac) {
            $this->absoluteCoordsMap[$ac[0].','.$ac[1].','.$ac[2]] = true;
        }
    }

    public function fillRotations() {
        $posX = $this->relativeCoords;
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

        foreach ($sixRotations as $rotation) {
            $r2 = $r3 = $r4 = [];
            foreach ($rotation as $c) {
                list($x, $y, $z) = $c;
                $r2[] = [-$y, $x, $z];
                $r3[] = [-$x, -$y, $z];
                $r4[] = [$y, -$x, $z];
            }
            $this->rotations = array_merge($this->rotations, [$rotation, $r2, $r3, $r4]);
        }
    }
}

function parseInput($input) {
    $scanners = [];
    foreach (explode("\n\n", $input) as $rawScanner) {
        $lines = explode("\n", $rawScanner);
        sscanf(array_shift($lines), "--- scanner %d ---", $number);
        $coords = [];
        foreach ($lines as $line) {
            $coords[] = sscanf($line, "%d,%d,%d");
        }
        $scanner = new Scanner();
        $scanner->number = $number;
        $scanner->relativeCoords = $coords;
        $scanner->fillRotations();
        $scanners[] = $scanner;
    }
    return $scanners;
}

function makeAbsoluteCoordsList($absolute, $relative, $relativeCoords) {
    $diff = [$absolute[0] - $relative[0], $absolute[1] - $relative[1], $absolute[2] - $relative[2]];
    $absCoords = [];
    foreach ($relativeCoords as $c) {
        $absCoords[] = [$diff[0] + $c[0], $diff[1] + $c[1], $diff[2] + $c[2]];
    }
    return $absCoords;
}

function findAbsoluteCoordsForScanner($undet, $settled) {
    foreach ($undet->rotations as $rotatedCoords) {
        foreach ($settled as $set) {
            foreach ($set->absoluteCoords as $absCoord) {
                foreach ($rotatedCoords as $relativeCoord) {
                    $unsettledAbsoluteCoords = makeAbsoluteCoordsList($absCoord, $relativeCoord, $rotatedCoords);
                    $matchingCount = 0;
                    foreach ($unsettledAbsoluteCoords as $ac) {
                        if (isset($set->absoluteCoordsMap[$ac[0].','.$ac[1].','.$ac[2]])) {
                            $matchingCount++;
                        }
                    }
                    if ($matchingCount >= 12) {
                        $undet->relativeCoords = $rotatedCoords;
                        $undet->absoluteCoords = $unsettledAbsoluteCoords;
                        $undet->fillAbsoluteCoordsMap();
                        $undet->x = $absCoord[0] - $relativeCoord[0];
                        $undet->y = $absCoord[1] - $relativeCoord[1];
                        $undet->z = $absCoord[2] - $relativeCoord[2];
                        return [$undet, true];
                    }
                }
            }
        }
    }
    return [$undet, false];
}

function solve($input) {
    $scanners = parseInput($input);
    $settled = [$scanners[0]];
    $settled[0]->absoluteCoords = $settled[0]->relativeCoords;
    $settled[0]->fillAbsoluteCoordsMap();
    $undetermined = array_slice($scanners, 1);

    while (!empty($undetermined)) {
        foreach ($undetermined as $i => $undet) {
            list($maybeUpdated, $ok) = findAbsoluteCoordsForScanner($undet, $settled);
            if ($ok) {
                $settled[] = $maybeUpdated;
                array_splice($undetermined, $i, 1);
                break;
            }
        }
    }

    $allBeacons = [];
    foreach ($settled as $s) {
        foreach ($s->absoluteCoordsMap as $key => $value) {
            $allBeacons[$key] = true;
        }
    }

    return count($allBeacons);
}

$input = trim(file_get_contents("input.txt"));
$result = solve($input);
echo $result . "\n";