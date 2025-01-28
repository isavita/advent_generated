
<?php

$rockstr = "####

 #
###
 #

  #
  #
###

#
#
#
#

##
##";

function getRocks() {
    global $rockstr;
    $rocks_str_arr = explode("\n\n", $rockstr);
    $rocks = [];
    foreach ($rocks_str_arr as $rock_str) {
        $rock = [];
        $lines = explode("\n", $rock_str);
        $height = count($lines);
        for ($y = 0; $y < $height; $y++) {
            for ($x = 0; $x < strlen($lines[$y]); $x++) {
                if ($lines[$y][$x] == '#') {
                    $rock[] = [$x, $height - 1 - $y];
                }
            }
        }
        $rocks[] = $rock;
    }
    return $rocks;
}

function readAll($path) {
    return trim(file_get_contents($path));
}

function collision($grid, $rock, $pos) {
    foreach ($rock as $p) {
        $px = $p[0] + $pos[0];
        $py = $p[1] + $pos[1];
        if (isset($grid["{$px},{$py}"]) || $px < 0 || $px > 6) {
            return true;
        }
    }
    return false;
}

function dirFromByte($b) {
    switch ($b) {
        case '<': return [-1, 0];
        case '>': return [1, 0];
        default: return [0, -1];
    }
}

$jetPattern = readAll("input.txt");
$rocks = getRocks();
$grid = [];
for ($x = 0; $x < 7; $x++) {
    $grid["{$x},0"] = 1;
}
$floor = 0;
$j = 0;
$repeat = [];

for ($i = 0, $curr = 0; ; $i++, $curr = ($curr + 1) % count($rocks)) {
    $key = "{$curr},{$j}";
    if (isset($repeat[$key])) {
        $r = $repeat[$key];
        $previ = $r[0];
        $prevFloor = $r[1];
        if ((1000000000000 - $i) % ($i - $previ) == 0) {
            echo $floor + (int)((1000000000000 - $i) / ($i - $previ)) * ($floor - $prevFloor) . "\n";
            break;
        }
    }
    $repeat[$key] = [$i, $floor];
    $currRock = $rocks[$curr];
    $pos = [2, $floor + 4];
    while (true) {
        $jet = $jetPattern[$j];
        $j = ($j + 1) % strlen($jetPattern);
        $dir = dirFromByte($jet);
        $pos = [$pos[0] + $dir[0], $pos[1] + $dir[1]];
        if (collision($grid, $currRock, $pos)) {
            $pos = [$pos[0] - $dir[0], $pos[1] - $dir[1]];
        }
        $pos = [$pos[0], $pos[1] - 1];
        if (collision($grid, $currRock, $pos)) {
            $pos = [$pos[0], $pos[1] + 1];
            foreach ($currRock as $p) {
                $gp = [$p[0] + $pos[0], $p[1] + $pos[1]];
                $grid["{$gp[0]},{$gp[1]}"] = 1;
                if ($gp[1] > $floor) {
                    $floor = $gp[1];
                }
            }
            break;
        }
    }
}
