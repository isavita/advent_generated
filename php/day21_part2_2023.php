<?php

function parseData($data) {
    $garden = [];
    $start = -1;
    foreach ($data as $y => $line) {
        for ($x = 0; $x < strlen($line); $x++) {
            $c = $line[$x];
            if ($c != '#') {
                $garden["$x,$y"] = true;
            }
            if ($c == 'S') {
                $start = "$x,$y";
            }
        }
    }

    if ($start == -1) {
        throw new Exception("No start found!");
    }

    return [$garden, $start];
}

function complexMod($num, $mod) {
    list($x, $y) = explode(',', $num);
    $x = (int)$x;
    $y = (int)$y;
    return (($x + 10 * $mod) % $mod) . ',' . (($y + 10 * $mod) % $mod);
}

function calculateNumEnds($garden, $start, $numIterations, $maxSize) {
    $queue = [$start => true];
    $done = [];

    for ($i = 0; $i < 3 * $maxSize; $i++) {
        if (($i % $maxSize) == (int)(($maxSize - 1) / 2)) {
            $done[] = count($queue);
        }
        if (count($done) == 3) {
            break;
        }

        $newQueue = [];
        foreach (['1,0', '-1,0', '0,1', '0,-1'] as $dir) {
            foreach ($queue as $point => $_) {
                list($dx, $dy) = explode(',', $dir);
                list($px, $py) = explode(',', $point);
                $newPoint = ($px + $dx) . ',' . ($py + $dy);
                $newPointMod = complexMod($newPoint, $maxSize);
                if (isset($garden[$newPointMod])) {
                    $newQueue[$newPoint] = true;
                }
            }
        }
        $queue = $newQueue;
    }

    $quadraticFunction = function ($n, $a, $b, $c) {
        return $a + $n * ($b - $a + (($n - 1) * ($c - 2 * $b + $a) / 2));
    };

    return $quadraticFunction(intdiv($numIterations, $maxSize), $done[0], $done[1], $done[2]);
}

$file = fopen("input.txt", "r");
if (!$file) {
    echo "Error opening file\n";
    exit;
}

$gardenInput = [];
while (($line = fgets($file)) !== false) {
    $gardenInput[] = trim($line);
}
fclose($file);

list($garden, $start) = parseData($gardenInput);
$maxSize = count($gardenInput);

$sum = calculateNumEnds($garden, $start, 26501365, $maxSize);
echo $sum . "\n";