
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

function applyGravity($moons, $axis) {
    for ($i = 0; $i < count($moons); $i++) {
        for ($j = $i + 1; $j < count($moons); $j++) {
            switch ($axis) {
                case "x":
                    if ($moons[$i]->pos->x > $moons[$j]->pos->x) {
                        $moons[$i]->vel->x--;
                        $moons[$j]->vel->x++;
                    } elseif ($moons[$i]->pos->x < $moons[$j]->pos->x) {
                        $moons[$i]->vel->x++;
                        $moons[$j]->vel->x--;
                    }
                    break;
                case "y":
                    if ($moons[$i]->pos->y > $moons[$j]->pos->y) {
                        $moons[$i]->vel->y--;
                        $moons[$j]->vel->y++;
                    } elseif ($moons[$i]->pos->y < $moons[$j]->pos->y) {
                        $moons[$i]->vel->y++;
                        $moons[$j]->vel->y--;
                    }
                    break;
                case "z":
                    if ($moons[$i]->pos->z > $moons[$j]->pos->z) {
                        $moons[$i]->vel->z--;
                        $moons[$j]->vel->z++;
                    } elseif ($moons[$i]->pos->z < $moons[$j]->pos->z) {
                        $moons[$i]->vel->z++;
                        $moons[$j]->vel->z--;
                    }
                    break;
            }
        }
    }
}

function applyVelocity($moons, $axis) {
    foreach ($moons as $moon) {
        switch ($axis) {
            case "x":
                $moon->pos->x += $moon->vel->x;
                break;
            case "y":
                $moon->pos->y += $moon->vel->y;
                break;
            case "z":
                $moon->pos->z += $moon->vel->z;
                break;
        }
    }
}

function findCycle($moons, $initialMoons, $axis) {
    $steps = 1;
    while (true) {
        applyGravity($moons, $axis);
        applyVelocity($moons, $axis);

        $match = true;
        foreach ($moons as $i => $m) {
            switch ($axis) {
                case "x":
                    if ($m->pos->x != $initialMoons[$i]->pos->x || $m->vel->x != $initialMoons[$i]->vel->x) {
                        $match = false;
                    }
                    break;
                case "y":
                    if ($m->pos->y != $initialMoons[$i]->pos->y || $m->vel->y != $initialMoons[$i]->vel->y) {
                        $match = false;
                    }
                    break;
                case "z":
                    if ($m->pos->z != $initialMoons[$i]->pos->z || $m->vel->z != $initialMoons[$i]->vel->z) {
                        $match = false;
                    }
                    break;
            }
        }

        if ($match) {
            return $steps;
        }

        $steps++;
    }
}

function lcm($a, $b) {
    $bigA = gmp_init($a);
    $bigB = gmp_init($b);
    return gmp_strval(gmp_div(gmp_mul($bigA, $bigB), gmp_gcd($bigA, $bigB)));
}

$moons = [];
$initialMoons = [];

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);
foreach ($lines as $line) {
    sscanf($line, "<x=%d, y=%d, z=%d>", $x, $y, $z);
    $moons[] = new Moon(new Vec3($x, $y, $z), new Vec3(0, 0, 0));
    $initialMoons[] = new Moon(new Vec3($x, $y, $z), new Vec3(0, 0, 0));
}

$cycleX = findCycle($moons, $initialMoons, "x");
$cycleY = findCycle($moons, $initialMoons, "y");
$cycleZ = findCycle($moons, $initialMoons, "z");

$lcmXY = lcm($cycleX, $cycleY);
$lcmXYZ = lcm($lcmXY, $cycleZ);

echo $lcmXYZ . PHP_EOL;
?>
