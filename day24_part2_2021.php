
<?php

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);

$k = [];
$l = [];
$m = [];

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

for ($i = 0; $i < count($l); $i++) {
    switch ($l[$i]) {
        case 1:
            array_push($stack, $i);
            break;
        case 26:
            $pop = array_pop($stack);
            $constraints[$pop] = [$i, $m[$pop] + $k[$i]];
            break;
    }
}

$min = array_fill(0, 14, 0);

for ($i = 0; $i < 14; $i++) {
    if (!array_key_exists($i, $constraints)) {
        continue;
    }
    $vmin = 1;
    while ($vmin + $constraints[$i][1] < 1) {
        $vmin++;
    }
    $min[$i] = $vmin;
    $min[$constraints[$i][0]] = $vmin + $constraints[$i][1];
}

echo num($min) . PHP_EOL;

function num($w) {
    $n = 0;
    foreach ($w as $val) {
        $n *= 10;
        $n += $val;
    }
    return $n;
}
