
<?php

class Position {
    public $x;
    public $y;
    public $risk;

    public function __construct($x, $y, $risk) {
        $this->x = $x;
        $this->y = $y;
        $this->risk = $risk;
    }
}

class PriorityQueue extends SplPriorityQueue {
    public function compare($priority1, $priority2) {
        return $priority1 - $priority2;
    }
}

function dijkstra($grid) {
    $pq = new PriorityQueue();
    $pq->insert(new Position(0, 0, 0), 0);

    $rows = count($grid);
    $cols = count($grid[0]);
    $dist = array_fill(0, $rows, array_fill(0, $cols, PHP_INT_MAX));
    $dist[0][0] = 0;

    $directions = [new Position(1, 0, 0), new Position(0, 1, 0), new Position(-1, 0, 0), new Position(0, -1, 0)];

    while (!$pq->isEmpty()) {
        $curr = $pq->extract();
        if ($curr->x == $rows - 1 && $curr->y == $cols - 1) {
            return $curr->risk;
        }
        foreach ($directions as $d) {
            $nx = $curr->x + $d->x;
            $ny = $curr->y + $d->y;
            if ($nx >= 0 && $ny >= 0 && $nx < $rows && $ny < $cols) {
                $nextRisk = $curr->risk + $grid[$nx][$ny];
                if ($nextRisk < $dist[$nx][$ny]) {
                    $dist[$nx][$ny] = $nextRisk;
                    $pq->insert(new Position($nx, $ny, $nextRisk), -$nextRisk);
                }
            }
        }
    }
    return -1;
}

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);
$grid = [];
foreach ($lines as $line) {
    $row = array_map('intval', str_split($line));
    $grid[] = $row;
}

echo dijkstra($grid) . PHP_EOL;
?>
