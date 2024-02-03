
<?php

class Reindeer {
    public $speed;
    public $flyTime;
    public $restTime;
    public $distance = 0;
    public $flying = true;
    public $timeInMode = 0;
}

$reindeers = readReindeerDetails("input.txt");
simulateRace($reindeers, 2503);
$maxDistance = findMaxDistance($reindeers);
echo $maxDistance . PHP_EOL;

function readReindeerDetails($filename) {
    $file = fopen($filename, "r");
    $reindeers = [];

    while (!feof($file)) {
        $parts = explode(" ", trim(fgets($file)));
        $speed = (int)$parts[3];
        $flyTime = (int)$parts[6];
        $restTime = (int)$parts[13];

        $reindeer = new Reindeer();
        $reindeer->speed = $speed;
        $reindeer->flyTime = $flyTime;
        $reindeer->restTime = $restTime;

        $reindeers[] = $reindeer;
    }

    fclose($file);
    return $reindeers;
}

function simulateRace(&$reindeers, $totalSeconds) {
    for ($i = 0; $i < $totalSeconds; $i++) {
        foreach ($reindeers as $key => $reindeer) {
            if ($reindeer->flying) {
                $reindeer->distance += $reindeer->speed;
                $reindeer->timeInMode++;
                if ($reindeer->timeInMode == $reindeer->flyTime) {
                    $reindeer->flying = false;
                    $reindeer->timeInMode = 0;
                }
            } else {
                $reindeer->timeInMode++;
                if ($reindeer->timeInMode == $reindeer->restTime) {
                    $reindeer->flying = true;
                    $reindeer->timeInMode = 0;
                }
            }
        }
    }
}

function findMaxDistance($reindeers) {
    $maxDistance = 0;
    foreach ($reindeers as $reindeer) {
        if ($reindeer->distance > $maxDistance) {
            $maxDistance = $reindeer->distance;
        }
    }
    return $maxDistance;
}
?>
