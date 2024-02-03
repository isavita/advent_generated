
<?php

class Point {
    public $x;
    public $y;

    public function __construct($x, $y) {
        $this->x = $x;
        $this->y = $y;
    }
}

function isWall($favoriteNumber, $x, $y) {
    $num = $x*$x + 3*$x + 2*$x*$y + $y + $y*$y + $favoriteNumber;
    $bits = 0;
    while ($num > 0) {
        if ($num % 2 == 1) {
            $bits++;
        }
        $num = (int)($num / 2);
    }
    return $bits % 2 != 0;
}

function bfs($start, $target, $favoriteNumber) {
    $visited = [];
    $queue = [$start];
    $steps = 0;

    while (count($queue) > 0) {
        $size = count($queue);
        for ($i = 0; $i < $size; $i++) {
            $point = $queue[$i];
            if ($point->x == $target->x && $point->y == $target->y) {
                return $steps;
            }

            $deltas = [new Point(1, 0), new Point(-1, 0), new Point(0, 1), new Point(0, -1)];
            foreach ($deltas as $delta) {
                $next = new Point($point->x + $delta->x, $point->y + $delta->y);
                if ($next->x >= 0 && $next->y >= 0 && !isWall($favoriteNumber, $next->x, $next->y) && !isset($visited[$next->x][$next->y])) {
                    $visited[$next->x][$next->y] = true;
                    $queue[] = $next;
                }
            }
        }
        $queue = array_slice($queue, $size);
        $steps++;
    }

    return -1;
}

$input = file_get_contents("input.txt");
$favoriteNumber = trim($input);
$start = new Point(1, 1);
$target = new Point(31, 39);
$steps = bfs($start, $target, $favoriteNumber);
echo $steps . PHP_EOL;
?>
