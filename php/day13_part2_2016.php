
<?php

$input = file_get_contents("input.txt");
$favoriteNumber = intval($input);

class Point {
    public $x;
    public $y;

    public function __construct($x, $y) {
        $this->x = $x;
        $this->y = $y;
    }
}

function isWall($x, $y, $favoriteNumber) {
    $num = $x*$x + 3*$x + 2*$x*$y + $y + $y*$y + $favoriteNumber;
    $bits = 0;
    while ($num > 0) {
        if ($num % 2 == 1) {
            $bits++;
        }
        $num = intval($num / 2);
    }
    return $bits % 2 != 0;
}

function bfsMaxSteps($start, $maxSteps, $favoriteNumber) {
    $visited = [];
    $queue = [$start];
    $visited[serialize($start)] = true;
    $steps = 0;

    while (count($queue) > 0 && $steps < $maxSteps) {
        $size = count($queue);
        for ($i = 0; $i < $size; $i++) {
            $point = $queue[$i];

            $deltas = [new Point(1, 0), new Point(-1, 0), new Point(0, 1), new Point(0, -1)];
            foreach ($deltas as $delta) {
                $next = new Point($point->x + $delta->x, $point->y + $delta->y);
                if ($next->x >= 0 && $next->y >= 0 && !isWall($next->x, $next->y, $favoriteNumber) && !isset($visited[serialize($next)])) {
                    $visited[serialize($next)] = true;
                    $queue[] = $next;
                }
            }
        }
        $queue = array_slice($queue, $size);
        $steps++;
    }

    return count($visited);
}

$start = new Point(1, 1);
$reachableLocations = bfsMaxSteps($start, 50, $favoriteNumber);
echo $reachableLocations . "\n";

?>
