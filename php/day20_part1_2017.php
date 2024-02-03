
<?php

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);

$particles = [];
foreach ($lines as $line) {
    $parts = explode(", ", $line);

    $p = $v = $a = [0, 0, 0];
    foreach ($parts as $i => $part) {
        $coords = explode(",", substr($part, 3, -1));
        foreach ($coords as $j => $coord) {
            $num = (int)$coord;
            switch ($i) {
                case 0:
                    $p[$j] = $num;
                    break;
                case 1:
                    $v[$j] = $num;
                    break;
                case 2:
                    $a[$j] = $num;
                    break;
            }
        }
    }
    $particles[] = ['p' => $p, 'v' => $v, 'a' => $a];
}

$closestParticle = 0;
$minAccel = PHP_INT_MAX;
$minVelocity = PHP_INT_MAX;
$minPosition = PHP_INT_MAX;

foreach ($particles as $i => $particle) {
    $accel = array_sum(array_map('abs', $particle['a']));
    $velocity = array_sum(array_map('abs', $particle['v']));
    $position = array_sum(array_map('abs', $particle['p']));

    if ($accel < $minAccel || ($accel == $minAccel && $velocity < $minVelocity) ||
        ($accel == $minAccel && $velocity == $minVelocity && $position < $minPosition)) {
        $minAccel = $accel;
        $minVelocity = $velocity;
        $minPosition = $position;
        $closestParticle = $i;
    }
}

echo $closestParticle . PHP_EOL;
?>
