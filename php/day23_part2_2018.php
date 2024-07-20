
<?php

class Coordinate {
    public $x, $y, $z;

    public function __construct($x, $y, $z) {
        $this->x = $x;
        $this->y = $y;
        $this->z = $z;
    }

    public function distance($a) {
        return abs($this->x - $a->x) + abs($this->y - $a->y) + abs($this->z - $a->z);
    }
}

class Bots {
    private $bots = [];

    public function add($coordinate, $radius) {
        $this->bots[] = ['coordinate' => $coordinate, 'radius' => $radius];
    }

    public function haveInRange($pos) {
        $sum = 0;
        foreach ($this->bots as $bot) {
            if ($pos->distance($bot['coordinate']) <= $bot['radius']) {
                $sum++;
            }
        }
        return $sum;
    }

    public function getBots() {
        return $this->bots;
    }
}

function newBots($input) {
    $bots = new Bots();
    foreach ($input as $data) {
        preg_match('/pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/', $data, $matches);
        $bots->add(new Coordinate((int)$matches[1], (int)$matches[2], (int)$matches[3]), (int)$matches[4]);
    }
    return $bots;
}

function closestSuccess($bots) {
    $cur = new Coordinate(0, 0, 0);
    $topLeft = new Coordinate(0, 0, 0);
    $bottomRight = new Coordinate(0, 0, 0);
    $zoom = 1 << (PHP_INT_SIZE * 8 - 2);

    while ($zoom > 0) {
        $zoomedBots = new Bots();
        foreach ($bots->getBots() as $bot) {
            $c = $bot['coordinate'];
            $zoomedBots->add(new Coordinate(intval($c->x / $zoom), intval($c->y / $zoom), intval($c->z / $zoom)), intval($bot['radius'] / $zoom));
        }

        $best = ['pos' => null, 'count' => 0];

        for ($x = $topLeft->x; $x <= $bottomRight->x; $x++) {
            for ($y = $topLeft->y; $y <= $bottomRight->y; $y++) {
                for ($z = $topLeft->z; $z <= $bottomRight->z; $z++) {
                    $c = $zoomedBots->haveInRange(new Coordinate($x, $y, $z));

                    if ($c > $best['count'] || ($c === $best['count'] && ($best['pos'] === null || (new Coordinate(0, 0, 0))->distance(new Coordinate($x, $y, $z)) < (new Coordinate(0, 0, 0))->distance($best['pos'])))) {
                        $best['pos'] = new Coordinate($x, $y, $z);
                        $best['count'] = $c;
                    }
                }
            }
        }

        $topLeft->x = ($best['pos']->x - 1) << 1;
        $topLeft->y = ($best['pos']->y - 1) << 1;
        $topLeft->z = ($best['pos']->z - 1) << 1;
        $bottomRight->x = ($best['pos']->x + 1) << 1;
        $bottomRight->y = ($best['pos']->y + 1) << 1;
        $bottomRight->z = ($best['pos']->z + 1) << 1;
        $zoom >>= 1;
    }

    return (new Coordinate(0, 0, 0))->distance($best['pos']);
}

$input = file('input.txt', FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
$bots = newBots($input);
echo closestSuccess($bots);
