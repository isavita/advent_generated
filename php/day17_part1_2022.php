<?php

const ROCKSTR = "####\n\n # \n###\n # \n\n  #\n  #\n###\n\n#\n#\n#\n#\n\n##\n##";

function main() {
    $jetPattern = str_split(trim(file_get_contents("input.txt")));
    $rocks = getRocks();
    $grid = [];
    for ($x = 0; $x < 7; $x++) {
        $grid[pointToKey([$x, 0])] = true;
    }
    $floor = 0;
    $j = 0;
    $repeat = [];

    for ($i = 0, $curr = 0; ; $i++, $curr = ($curr + 1) % count($rocks)) {
        if ($i == 2022) {
            echo $floor;
            break;
        }
        $key = [$curr, $j];
        $repeat[$key[0] . "," . $key[1]] = [$i, $floor];
        $currRock = $rocks[$curr];
        $pos = [2, $floor + 4];
        while (true) {
            $jet = $jetPattern[$j];
            $j = ($j + 1) % count($jetPattern);
            $pos = addPoints($pos, dirFromByte($jet));
            if (collision($grid, $currRock, $pos)) {
                $pos = subPoints($pos, dirFromByte($jet));
            }
            $pos = addPoints($pos, [0, -1]);
            if (collision($grid, $currRock, $pos)) {
                $pos = subPoints($pos, [0, -1]);
                foreach ($currRock as $p) {
                    $grid[pointToKey(addPoints($p, $pos))] = true;
                    if (addPoints($p, $pos)[1] > $floor) {
                        $floor = addPoints($p, $pos)[1];
                    }
                }
                break;
            }
        }
    }
}

function collision($grid, $rock, $pos) {
    foreach ($rock as $p) {
        $p = addPoints($p, $pos);
        if (isset($grid[pointToKey($p)]) || $p[0] < 0 || $p[0] > 6) {
            return true;
        }
    }
    return false;
}

function getRocks() {
    $rocks = [];
    foreach (explode("\n\n", ROCKSTR) as $i => $rock) {
        $rocks[$i] = [];
        $lines = explode("\n", $rock);
        foreach ($lines as $y => $line) {
            for ($x = 0; $x < strlen($line); $x++) {
                if ($line[$x] === '#') {
                    $rocks[$i][] = [$x, count($lines) - 1 - $y];
                }
            }
        }
    }
    return $rocks;
}

function pointToKey($point) {
    return $point[0] . "," . $point[1];
}

function addPoints($p1, $p2) {
    return [$p1[0] + $p2[0], $p1[1] + $p2[1]];
}

function subPoints($p1, $p2) {
    return [$p1[0] - $p2[0], $p1[1] - $p2[1]];
}

function dirFromByte($b) {
    switch ($b) {
        case 'N':
        case '^':
        case 'U':
            return [0, 1];
        case 'E':
        case '>':
        case 'R':
            return [1, 0];
        case 'S':
        case 'v':
        case 'D':
            return [0, -1];
        case 'W':
        case '<':
        case 'L':
            return [-1, 0];
    }
}

main();

?>