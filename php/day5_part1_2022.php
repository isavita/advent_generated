
<?php

$input = file_get_contents("input.txt");
$s = explode("\n\n", $input);
$input = explode("\n", $s[0]);
$stacks = array_fill(0, (strlen($input[0])+1)/4, []);

foreach ($input as $line) {
    $lineArr = str_split($line);
    foreach ($lineArr as $i => $b) {
        if (ord($b) >= ord('A') && ord($b) <= ord('Z')) {
            array_push($stacks[($i-1)/4], $b);
        }
    }
}

$steps = explode("\n", $s[1]);
echo move($stacks, $steps);

function move($st, $steps) {
    $stacks = array_map(function($stack) {
        return array_reverse($stack);
    }, $st);

    foreach ($steps as $step) {
        sscanf($step, "move %d from %d to %d", $n, $from, $to);
        $from--;
        $to--;
        for ($i = 0; $i < $n; $i++) {
            array_push($stacks[$to], array_pop($stacks[$from]));
        }
    }

    $b = array_map(function($stack) {
        return end($stack);
    }, $stacks);

    return implode("", $b);
}
