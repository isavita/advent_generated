
<?php

class Reindeer {
    public $speed;
    public $flyTime;
    public $restTime;
    public $distance = 0;
    public $points = 0;
    public $flying = true;
    public $timeInMode = 0;
}

$reindeers = readReindeerDetails("input.txt");
simulateRaceWithPoints($reindeers, 2503);
$maxPoints = findMaxPoints($reindeers);
echo $maxPoints . PHP_EOL;

function readReindeerDetails($filename) {
    $file = fopen($filename, "r");
    $reindeers = [];

    while (!feof($file)) {
        $line = fgets($file);
        $parts = explode(" ", $line);
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

function simulateRaceWithPoints(&$reindeers, $totalSeconds) {
    for ($i = 0; $i < $totalSeconds; $i++) {
        $maxDistance = 0;
        foreach ($reindeers as $reindeer) {
            if ($reindeer->flying) {
                $reindeer->distance += $reindeer->speed;
            }
            $reindeer->timeInMode++;
            if (($reindeer->flying && $reindeer->timeInMode == $reindeer->flyTime) || (!$reindeer->flying && $reindeer->timeInMode == $reindeer->restTime)) {
                $reindeer->flying = !$reindeer->flying;
                $reindeer->timeInMode = 0;
            }
            if ($reindeer->distance > $maxDistance) {
                $maxDistance = $reindeer->distance;
            }
        }
        foreach ($reindeers as $reindeer) {
            if ($reindeer->distance == $maxDistance) {
                $reindeer->points++;
            }
        }
    }
}

function findMaxPoints($reindeers) {
    $maxPoints = 0;
    foreach ($reindeers as $reindeer) {
        if ($reindeer->points > $maxPoints) {
            $maxPoints = $reindeer->points;
        }
    }
    return $maxPoints;
}
?>
