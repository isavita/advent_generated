
<?php

$input = file_get_contents("input.txt");
$args = explode("\n", $input);
$graph = [];

foreach ($args as $line) {
    if ($line == "") {
        continue;
    }
    $graph[] = str_split($line);
}

$H = count($graph);
$W = count($graph[0]);

$move = [
    ["label" => "left", "x" => -1, "y" => 0],
    ["label" => "up", "x" => 0, "y" => -1],
    ["label" => "right", "x" => 1, "y" => 0],
    ["label" => "down", "x" => 0, "y" => 1],
];

$sum = 0;

for ($y = 0; $y < $H; $y++) {
    for ($x = 0; $x < $W; $x++) {
        if ($graph[$y][$x] == ".") {
            continue;
        }

        $area = 0;
        $target = $graph[$y][$x];
        $visited = [];
        $side = [];

        $search = function (&$search, $cx, $cy, $label) use (&$graph, $target, &$visited, &$side, $move, $H, $W) {
            if (!isset($graph[$cy][$cx]) || $graph[$cy][$cx] != $target) {
                if ($label != "" && !isset($visited[$cx . "," . $cy])) {
                    saveOuter($label, $side, $cx, $cy);
                }
                return;
            }

            $visited[$cx . "," . $cy] = true;
            $area = &$GLOBALS['area'];
            $area++;
            $graph[$cy][$cx] = ".";

            foreach ($move as $m) {
                $nx = $cx + $m["x"];
                $ny = $cy + $m["y"];

                if ($nx < 0 || $nx >= $W || $ny < 0 || $ny >= $H) {
                    saveOuter($m["label"], $side, $nx, $ny);
                    continue;
                }
                $search($search, $nx, $ny, $m["label"]);
            }
        };

        $search($search, $x, $y, "");
        $outer = countOuter($side);
        $sum += $area * $outer;
    }
}
echo $sum . PHP_EOL;

function saveOuter($label, &$side, $x, $y) {
    if ($label == "up" || $label == "down") {
        $key = $y . ":" . $x;
    } else {
        $key = $x . ":" . $y;
    }

    if (!isset($side[$label])) {
        $side[$label] = [];
    }
    $side[$label][$key] = true;
}

function countOuter($side) {
    $outer = 0;
    foreach ($side as $label => $keys) {
        $array = array_keys($keys);
        usort($array, function ($a, $b) {
            $aParts = explode(":", $a);
            $bParts = explode(":", $b);
            $aFirst = (int)$aParts[0];
            $bFirst = (int)$bParts[0];
            if ($aFirst == $bFirst) {
                $aSecond = (int)$aParts[1];
                $bSecond = (int)$bParts[1];
                return $aSecond <=> $bSecond;
            }
            return $aFirst <=> $bFirst;
        });

        $temp = [];
        foreach ($array as $current) {
            $parts = explode(":", $current);
            $i = (int)$parts[0];
            $j = (int)$parts[1];
            if (!check($temp, $i, $j)) {
                $outer++;
            }
            $temp[] = $current;
        }
    }
    return $outer;
}

function check($ary, $i, $j) {
    $search = [
        $i . ":" . ($j - 1),
        $i . ":" . ($j + 1),
    ];
    foreach ($search as $s) {
        foreach ($ary as $a) {
            if ($a == $s) {
                return true;
            }
        }
    }
    return false;
}
