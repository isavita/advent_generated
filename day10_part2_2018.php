
<?php

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);

$head = new Star();
$tail = $head;

$re = '/position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>/';

foreach ($lines as $line) {
    preg_match($re, $line, $split);
    if (count($split) != 5) {
        continue;
    }
    $star = new Star();
    $star->x = (int)$split[1];
    $star->y = (int)$split[2];
    $star->vX = (int)$split[3];
    $star->vY = (int)$split[4];
    $tail->next = $star;
    $tail = $star;
}

$smallestT = 0;
$smallestArea = PHP_INT_MAX;

for ($t = 1; $t < 100000; $t++) {
    $maxX = 0;
    $maxY = 0;
    $minX = 0;
    $minY = 0;

    for ($temp = $head->next; $temp->next != null; $temp = $temp->next) {
        $x = $temp->x + $temp->vX * $t;
        if ($maxX < $x) {
            $maxX = $x;
        } elseif ($minX > $x) {
            $minX = $x;
        }
        $y = $temp->y + $temp->vY * $t;
        if ($maxY < $y) {
            $maxY = $y;
        } elseif ($minY > $y) {
            $minY = $y;
        }
    }

    $lenX = $maxX - $minX + 1;
    $lenY = $maxY - $minY + 1;
    $area = $lenX + $lenY;

    if ($smallestArea > $area) {
        $smallestArea = $area;
        $smallestT = $t;
    }
}

echo $smallestT . "\n";

$t = $smallestT;

$maxX = 0;
$maxY = 0;
$minX = 0;
$minY = 0;

for ($temp = $head->next; $temp->next != null; $temp = $temp->next) {
    $temp->x = $temp->x + $temp->vX * $t;
    if ($maxX < $temp->x) {
        $maxX = $temp->x;
    } elseif ($minX > $temp->x) {
        $minX = $temp->x;
    }
    $temp->y = $temp->y + $temp->vY * $t;
    if ($maxY < $temp->y) {
        $maxY = $temp->y;
    } elseif ($minY > $temp->y) {
        $minY = $temp->y;
    }
}

$mapper = array_fill(0, $maxY - $minY + 1, array_fill(0, $maxX - $minX + 1, false));

foreach ($head->next as $temp) {
    $mapper[$temp->y][$temp->x] = true;
}

foreach ($mapper as $row) {
    foreach ($row as $cell) {
    }
}

class Star {
    public $x;
    public $y;
    public $vX;
    public $vY;
    public $next;
}
