<?php
class Vec3 {
    public $x, $y, $z;

    public function __construct($x, $y, $z) {
        $this->x = $x;
        $this->y = $y;
        $this->z = $z;
    }
}

class Moon {
    public $pos, $vel;

    public function __construct($pos, $vel) {
        $this->pos = $pos;
        $this->vel = $vel;
    }
}

function myAbs($x) {
    return $x < 0 ? -$x : $x;
}

function applyGravity($moons) {
    for ($i = 0; $i < count($moons); $i++) {
        for ($j = $i + 1; $j < count($moons); $j++) {
            if ($moons[$i]->pos->x > $moons[$j]->pos->x) {
                $moons[$i]->vel->x--;
                $moons[$j]->vel->x++;
            } elseif ($moons[$i]->pos->x < $moons[$j]->pos->x) {
                $moons[$i]->vel->x++;
                $moons[$j]->vel->x--;
            }

            if ($moons[$i]->pos->y > $moons[$j]->pos->y) {
                $moons[$i]->vel->y--;
                $moons[$j]->vel->y++;
            } elseif ($moons[$i]->pos->y < $moons[$j]->pos->y) {
                $moons[$i]->vel->y++;
                $moons[$j]->vel->y--;
            }

            if ($moons[$i]->pos->z > $moons[$j]->pos->z) {
                $moons[$i]->vel->z--;
                $moons[$j]->vel->z++;
            } elseif ($moons[$i]->pos->z < $moons[$j]->pos->z) {
                $moons[$i]->vel->z++;
                $moons[$j]->vel->z--;
            }
        }
    }
}

function applyVelocity($moons) {
    foreach ($moons as $moon) {
        $moon->pos->x += $moon->vel->x;
        $moon->pos->y += $moon->vel->y;
        $moon->pos->z += $moon->vel->z;
    }
}

function totalEnergy($moons) {
    $total = 0;
    foreach ($moons as $moon) {
        $pot = myAbs($moon->pos->x) + myAbs($moon->pos->y) + myAbs($moon->pos->z);
        $kin = myAbs($moon->vel->x) + myAbs($moon->vel->y) + myAbs($moon->vel->z);
        $total += $pot * $kin;
    }
    return $total;
}

$file = fopen("input.txt", "r");
$moons = [];
while (($line = fgets($file)) !== false) {
    if (preg_match('/^<x=(-?\d+), y=(-?\d+), z=(-?\d+)>$/', $line, $matches)) {
        $moons[] = new Moon(new Vec3((int)$matches[1], (int)$matches[2], (int)$matches[3]), new Vec3(0, 0, 0));
    }
}
fclose($file);

for ($step = 0; $step < 1000; $step++) {
    applyGravity($moons);
    applyVelocity($moons);
}

echo totalEnergy($moons) . "\n";