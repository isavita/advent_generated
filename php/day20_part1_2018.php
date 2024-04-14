<?php

class Point {
    public $x;
    public $y;

    public function __construct($x, $y) {
        $this->x = $x;
        $this->y = $y;
    }

    public function __toString() {
        return $this->x . "," . $this->y;
    }
}

function buildMap($regex) {
    $dm = [];
    $stack = [];
    $cp = new Point(0, 0);
    for ($i = 0; $i < strlen($regex); $i++) {
        $c = $regex[$i];
        if ($c == '(') {
            array_push($stack, $cp);
        } elseif ($c == '|') {
            $cp = end($stack);
        } elseif ($c == ')') {
            $cp = array_pop($stack);
        } else {
            $np = move($cp, $c);
            if (!isset($dm["$cp"])) {
                $dm["$cp"] = [];
            }
            $dm["$cp"]["$np"] = true;
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

function findFurthestRoom($dm) {
    $visited = [];
    $queue = [];
    array_push($queue, new Point(0, 0));
    $maxDoors = 0;

    while (count($queue) > 0) {
        $p = array_shift($queue);
        if (isset($dm["$p"])) {
            foreach ($dm["$p"] as $npStr => $_) {
                $np = new Point(explode(",", $npStr)[0], explode(",", $npStr)[1]);
                if (!isset($visited["$np"])) {
                    $visited["$np"] = $visited["$p"] + 1;
                    $maxDoors = max($maxDoors, $visited["$np"]);
                    array_push($queue, $np);
                }
            }
        }
    }
    return $maxDoors;
}

$data = file_get_contents("input.txt");
$regex = substr(trim($data), 1, -1);
$dm = buildMap($regex);
$maxDoors = findFurthestRoom($dm);
echo $maxDoors;

?>