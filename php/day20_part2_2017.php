<?php

class Particle {
    public $p = [0, 0, 0];
    public $v = [0, 0, 0];
    public $a = [0, 0, 0];
}

function readParticles($filename) {
    $file = fopen($filename, "r");
    $particles = [];

    while ($line = fgets($file)) {
        $line = trim($line);
        $parts = explode(", ", $line);
        $particle = new Particle();

        foreach ($parts as $i => $part) {
            $coords = explode(",", substr($part, 3, -1));
            foreach ($coords as $j => $coord) {
                switch ($i) {
                    case 0:
                        $particle->p[$j] = (int)$coord;
                        break;
                    case 1:
                        $particle->v[$j] = (int)$coord;
                        break;
                    case 2:
                        $particle->a[$j] = (int)$coord;
                        break;
                }
            }
        }

        $particles[] = $particle;
    }

    fclose($file);
    return $particles;
}

function simulateParticles(&$particles) {
    for ($tick = 0; $tick < 1000; $tick++) {
        $positions = [];

        foreach ($particles as $i => $particle) {
            for ($j = 0; $j < 3; $j++) {
                $particle->v[$j] += $particle->a[$j];
                $particle->p[$j] += $particle->v[$j];
            }

            $posStr = implode(",", $particle->p);
            if (!isset($positions[$posStr])) {
                $positions[$posStr] = 0;
            }
            $positions[$posStr]++;
        }

        $newParticles = [];
        foreach ($particles as $particle) {
            $posStr = implode(",", $particle->p);
            if ($positions[$posStr] === 1) {
                $newParticles[] = $particle;
            }
        }

        $particles = $newParticles;
    }
}

$particles = readParticles("input.txt");
simulateParticles($particles);
echo count($particles) . PHP_EOL;

?>