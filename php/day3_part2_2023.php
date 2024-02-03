
<?php

$input = file_get_contents("input.txt");
$input = trim($input);
$lines = explode("\n", $input);

$grid = [];
$parts = [];
$curr = null;

foreach ($lines as $y => $line) {
    if ($curr !== null) {
        $parts[] = $curr;
        $curr = null;
    }
    for ($x = 0; $x < strlen($line); $x++) {
        $c = $line[$x];
        $grid["$x,$y"] = $c;
        if ($c >= '0' && $c <= '9') {
            if ($curr === null) {
                $curr = ['y' => $y, 'xmin' => $x, 'xmax' => $x, 'n' => ord($c) - ord('0')];
            } else {
                $curr['n'] *= 10;
                $curr['n'] += ord($c) - ord('0');
                $curr['xmax'] = $x;
            }
        } elseif ($curr !== null) {
            $parts[] = $curr;
            $curr = null;
        }
    }
}

$partsGrid = [];
foreach ($parts as $i => $p) {
    for ($x = $p['xmin']; $x <= $p['xmax']; $x++) {
        $partsGrid["$x,{$p['y']}"] = $i;
    }
}

$sum = 0;
$neighbors8 = [[0, 1], [0, -1], [1, 0], [-1, 0], [-1, -1], [-1, 1], [1, -1], [1, 1]];

foreach ($grid as $p => $c) {
    if ($c === '*') {
        $neighborParts = [];
        foreach ($neighbors8 as $n) {
            $neighbor = [$n[0] + explode(',', $p)[0], $n[1] + explode(',', $p)[1]];
            if (array_key_exists("$neighbor[0],$neighbor[1]", $partsGrid)) {
                $neighborParts[$partsGrid["$neighbor[0],$neighbor[1]"]] = true;
            }
        }
        if (count($neighborParts) === 2) {
            $prod = 1;
            foreach (array_keys($neighborParts) as $i) {
                $prod *= $parts[$i]['n'];
            }
            $sum += $prod;
        }
    }
}

echo $sum . "\n";
?>
