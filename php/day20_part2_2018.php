<?php

class Point {
    public $x;
    public $y;

    public function __construct($x, $y) {
        $this->x = $x;
        $this->y = $y;
    }

    public function getKey() {
        return "{$this->x},{$this->y}";
    }
}

function buildMap($regex) {
    $dm = [];
    $stack = [];
    $cp = new Point(0, 0);
    $len = strlen($regex);
    for ($i = 0; $i < $len; $i++) {
        $c = $regex[$i];
        if ($c == '(') {
            array_push($stack, $cp);
        } elseif ($c == '|') {
            $cp = end($stack);
        } elseif ($c == ')') {
            $cp = end($stack);
            array_pop($stack);
        } else {
            $np = move($cp, $c);
            if (!isset($dm[$cp->getKey()])) {
                $dm[$cp->getKey()] = [];
            }
            $dm[$cp->getKey()][$np->getKey()] = true;
            $cp = $np;
        }
    }
    return $dm;
}

function move($p, $dir) {
    switch ($dir) {
        case 'N':
            return new Point($p->x, $p->y - 1);
        case 'S':
            return new Point($p->x, $p->y + 1);
        case 'E':
            return new Point($p->x + 1, $p->y);
        case 'W':
            return new Point($p->x - 1, $p->y);
    }
    return $p;
}

function countRooms($dm, $minDoors) {
    $visited = [];
    $queue = [new Point(0, 0)];
    $roomCount = 0;

    while (!empty($queue)) {
        $p = array_shift($queue);
        $pKey = $p->getKey();
        foreach ($dm[$pKey] ?? [] as $npKey => $_) {
            if (!isset($visited[$npKey])) {
                $np = explode(',', $npKey);
                $npPoint = new Point($np[0], $np[1]);
                $visited[$npKey] = isset($visited[$pKey]) ? $visited[$pKey] + 1 : 1;
                if ($visited[$npKey] >= $minDoors) {
                    $roomCount++;
                }
                array_push($queue, $npPoint);
            }
        }
    }
    return $roomCount;
}

$data = file_get_contents("input.txt");
$regex = trim($data);
$dm = buildMap(substr($regex, 1, -1));
$rooms = countRooms($dm, 1000);
echo $rooms;
?>