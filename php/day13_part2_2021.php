<?php
$input = file('input.txt');

$dots = [];
$folds = [];

foreach ($input as $line) {
    if (trim($line) == '') continue;
    if (strpos($line, 'fold along') !== false) {
        $fold = explode('=', substr($line, 11));
        $folds[] = ['axis' => $fold[0], 'value' => (int)$fold[1]];
    } else {
        $dot = explode(',', $line);
        $dots[] = ['x' => (int)$dot[0], 'y' => (int)$dot[1]];
    }
}

$firstFold = $folds[0];
$newDots = [];
foreach ($dots as $dot) {
    if ($firstFold['axis'] == 'x') {
        if ($dot['x'] < $firstFold['value']) {
            $newDots[] = $dot;
        } else {
            $newDots[] = ['x' => $firstFold['value'] * 2 - $dot['x'], 'y' => $dot['y']];
        }
    } else {
        if ($dot['y'] < $firstFold['value']) {
            $newDots[] = $dot;
        } else {
            $newDots[] = ['x' => $dot['x'], 'y' => $firstFold['value'] * 2 - $dot['y']];
        }
    }
}

$uniqueDots = [];
foreach ($newDots as $dot) {
    $key = $dot['x'] . ',' . $dot['y'];
    $uniqueDots[$key] = $dot;
}

echo count($uniqueDots) . "\n";

$result = '';
foreach ($folds as $fold) {
    $newDots = [];
    foreach ($dots as $dot) {
        if ($fold['axis'] == 'x') {
            if ($dot['x'] < $fold['value']) {
                $newDots[] = $dot;
            } else {
                $newDots[] = ['x' => $fold['value'] * 2 - $dot['x'], 'y' => $dot['y']];
            }
        } else {
            if ($dot['y'] < $fold['value']) {
                $newDots[] = $dot;
            } else {
                $newDots[] = ['x' => $dot['x'], 'y' => $fold['value'] * 2 - $dot['y']];
            }
        }
    }
    $uniqueDots = [];
    foreach ($newDots as $dot) {
        $key = $dot['x'] . ',' . $dot['y'];
        $uniqueDots[$key] = $dot;
    }
    $dots = array_values($uniqueDots);
}

$maxX = 0;
$maxY = 0;
foreach ($dots as $dot) {
    if ($dot['x'] > $maxX) $maxX = $dot['x'];
    if ($dot['y'] > $maxY) $maxY = $dot['y'];
}

for ($y = 0; $y <= $maxY; $y++) {
    for ($x = 0; $x <= $maxX; $x++) {
        $key = $x . ',' . $y;
        if (isset($uniqueDots[$key])) {
            $result .= '#';
        } else {
            $result .= '.';
        }
    }
    $result .= "\n";
}

echo $result;
?>