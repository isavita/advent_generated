
<?php

function readAll($path) {
    $file = file_get_contents($path);
    if ($file === false) {
        throw new Exception("Error reading file");
    }
    return trim($file);
}

function num($w) {
    $n = 0;
    foreach ($w as $val) {
        $n *= 10;
        $n += $val;
    }
    return $n;
}

$input = readAll("input.txt");
$k = $l = $m = [];
$lines = explode("\n", $input);
foreach ($lines as $i => $line) {
    $v = 0;
    switch ($i % 18) {
        case 4:
            sscanf($line, "div z %d", $v);
            $l[] = $v;
            break;
        case 5:
            sscanf($line, "add x %d", $v);
            $k[] = $v;
            break;
        case 15:
            sscanf($line, "add y %d", $v);
            $m[] = $v;
            break;
    }
}

$constraints = [];
$stack = [];
foreach ($l as $i => $val) {
    switch ($val) {
        case 1:
            array_push($stack, $i);
            break;
        case 26:
            $pop = array_pop($stack);
            $constraints[$pop] = [$i, $m[$pop] + $k[$i]];
            break;
    }
}

$max = array_fill(0, 14, 0);
for ($i = 0; $i < 14; $i++) {
    if (!array_key_exists($i, $constraints)) {
        continue;
    }
    $vmax = 9;
    while ($vmax + $constraints[$i][1] > 9) {
        $vmax--;
    }
    $max[$i] = $vmax;
    $max[$constraints[$i][0]] = $vmax + $constraints[$i][1];
}

echo num($max) . PHP_EOL;

?>
