<?php
function readAll($path) {
    return file_get_contents($path);
}

function myAbs($x) {
    return $x < 0 ? -$x : $x;
}

$x = [1];
foreach (explode("\n", readAll("input.txt")) as $line) {
    switch ($line) {
        case "noop":
            $x[] = $x[count($x) - 1];
            break;
        default:
            $n = intval(substr($line, 5));
            $x[] = $x[count($x) - 1];
            $x[] = $x[count($x) - 1] + $n;
            break;
    }
}

$grid = [];
for ($i = 0; $i < count($x); $i++) {
    $crtx = $i % 40;
    $crty = (int)($i / 40);
    if (myAbs($crtx - $x[$i]) <= 1) {
        $grid[$crtx . "," . $crty] = "#";
    } else {
        unset($grid[$crtx . "," . $crty]);
    }
}

for ($y = 0; $y < 6; $y++) {
    for ($x = 0; $x < 40; $x++) {
        echo isset($grid[$x . "," . $y]) ? "#" : ".";
    }
    echo "\n";
}
?>