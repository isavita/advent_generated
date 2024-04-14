<?php

class Node {
    public $used;
    public $avail;
}

function readAll() {
    return trim(file_get_contents("input.txt"));
}

function minmoves($nodes) {
    list($w, $h) = dim($nodes);
    $goal = new Point($w, 0);
    $hole = findHole($nodes);
    
    $sum = 0;
    while ($goal->x !== 0 || $goal->y !== 0) {
        $next = new Point($goal->x - 1, $goal->y);
        $m = moves($nodes, $goal, $hole, $next);
        $sum += $m;
        $hole = $next;
        $m = moves($nodes, $goal, $goal, $hole);
        $sum += $m;
        $temp = $goal;
        $goal = $hole;
        $hole = $temp;
    }
    return $sum;
}

function findHole($nodes) {
    foreach ($nodes as $p => $n) {
        if ($n->used == 0) {
            return new Point(...explode(',', $p));
        }
    }
    throw new Exception("no hole");
}

function moves($nodes, $goal, $from, $to) {
    list($w, $h) = dim($nodes);
    $depth = [$from->key() => 0];
    $pq = new SplPriorityQueue();
    $pq->insert($from, 0);
    
    while (!$pq->isEmpty()) {
        $p = $pq->extract();
        if ($p == $to) {
            return $depth[$p->key()];
        }
        $currdepth = $depth[$p->key()] + 1;
        foreach (neighbors4() as $n) {
            $next = $p->add($n);
            if ($next->x < 0 || $next->y < 0 || $next->x > $w || $next->y > $h || $nodes[$next->key()]->used > 400 || $next == $goal) {
                continue;
            }
            if (!isset($depth[$next->key()]) || $currdepth < $depth[$next->key()]) {
                $depth[$next->key()] = $currdepth;
                $pq->insert($next, -$currdepth);
            }
        }
    }
    throw new Exception("no possible path");
}

function dim($nodes) {
    $w = $h = 0;
    foreach ($nodes as $p => $_) {
        list($x, $y) = explode(',', $p);
        if ($x > $w) $w = $x;
        if ($y > $h) $h = $y;
    }
    return [$w, $h];
}

function neighbors4() {
    return [new Point(0, 1), new Point(0, -1), new Point(1, 0), new Point(-1, 0)];
}

class Point {
    public $x, $y;
    function __construct($x, $y) {
        $this->x = $x;
        $this->y = $y;
    }
    function add($other) {
        return new Point($this->x + $other->x, $this->y + $other->y);
    }
    function key() {
        return "{$this->x},{$this->y}";
    }
}

$input = explode("\n", readAll());
array_shift($input); // Remove the first two lines which might not contain node data
array_shift($input);
$nodes = [];
foreach ($input as $line) {
    $f = preg_split('/\s+/', $line);
    preg_match('/-x(\d+)-y(\d+)/', $f[0], $matches);
    $n = new Node();
    $p = new Point((int)$matches[1], (int)$matches[2]);
    $n->used = (int)$f[2];
    $n->avail = (int)$f[3];
    $nodes[$p->key()] = $n;
}

echo minmoves($nodes);

?>