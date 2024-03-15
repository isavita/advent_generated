<?php
$cubes = [];
$neighbors = [
    [-1, 0, 0],
    [1, 0, 0],
    [0, -1, 0],
    [0, 1, 0],
    [0, 0, -1],
    [0, 0, 1],
];
$min = [PHP_INT_MAX, PHP_INT_MAX, PHP_INT_MAX];
$max = [PHP_INT_MIN, PHP_INT_MIN, PHP_INT_MIN];

$file = fopen("input.txt", "r");
while (($line = fgets($file)) !== false) {
    $line = trim($line);
    if ($line === "") {
        continue;
    }
    $cube = explode(",", $line);
    $cubes[] = [
        "x" => (int)$cube[0],
        "y" => (int)$cube[1],
        "z" => (int)$cube[2],
    ];
    $min[0] = min($min[0], $cubes[count($cubes) - 1]["x"]);
    $min[1] = min($min[1], $cubes[count($cubes) - 1]["y"]);
    $min[2] = min($min[2], $cubes[count($cubes) - 1]["z"]);
    $max[0] = max($max[0], $cubes[count($cubes) - 1]["x"]);
    $max[1] = max($max[1], $cubes[count($cubes) - 1]["y"]);
    $max[2] = max($max[2], $cubes[count($cubes) - 1]["z"]);
}
fclose($file);

$min[0]--;
$min[1]--;
$min[2]--;
$max[0]++;
$max[1]++;
$max[2]++;

$faces = 0;
$queue = [$min];
$seen = [$min];
while (count($queue) > 0) {
    $curr = array_shift($queue);
    foreach ($neighbors as $delta) {
        $next = [
            "x" => $curr["x"] + $delta[0],
            "y" => $curr["y"] + $delta[1],
            "z" => $curr["z"] + $delta[2],
        ];
        if ($next["x"] < $min[0] || $next["y"] < $min[1] || $next["z"] < $min[2] || $next["x"] > $max[0] || $next["y"] > $max[1] || $next["z"] > $max[2]) {
            continue;
        }
        if (in_array($next, $cubes, true)) {
            $faces++;
        } elseif (!in_array($next, $seen, true)) {
            $seen[] = $next;
            $queue[] = $next;
        }
    }
}
echo $faces . "\n";