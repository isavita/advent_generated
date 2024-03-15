<?php
$filename = "input.txt";

$deps = [];
$allSteps = [];

// Read and parse the input file
$lines = file($filename, FILE_IGNORE_NEW_LINES);
foreach ($lines as $line) {
    preg_match('/Step (\w) must be finished before step (\w) can begin./', $line, $matches);
    $a = $matches[1];
    $b = $matches[2];

    if (!isset($deps[$b])) {
        $deps[$b] = [];
    }
    $deps[$b][] = $a;

    if (!isset($allSteps[$a])) {
        $allSteps[$a] = ['id' => $a, 'duration' => ord($a) - ord('A') + 61];
    }
    if (!isset($allSteps[$b])) {
        $allSteps[$b] = ['id' => $b, 'duration' => ord($b) - ord('A') + 61];
    }
}

$numWorkers = 5;
$baseDuration = 60;

// Simulate the work process
$timeTaken = simulateWork($deps, $allSteps, $numWorkers, $baseDuration);
echo $timeTaken . "\n";

function simulateWork($deps, $allSteps, $numWorkers, $baseDuration)
{
    $workers = array_fill(0, $numWorkers, 0);
    $tasks = array_fill(0, $numWorkers, 0);
    $time = 0;

    while (!empty($allSteps)) {
        // Find available steps
        $available = [];
        foreach ($allSteps as $step => $data) {
            if (empty($deps[$step]) && !isBeingWorkedOn($step, $tasks)) {
                $available[] = $step;
            }
        }
        sort($available);

        // Assign available steps to free workers
        for ($i = 0; $i < $numWorkers; $i++) {
            if ($workers[$i] == 0 && !empty($available)) {
                $tasks[$i] = $available[0];
                $workers[$i] = $allSteps[$available[0]]['duration'];
                array_shift($available);
            }
        }

        // Advance time by the smallest non-zero duration among workers
        $minDuration = findMinDuration($workers);
        for ($i = 0; $i < $numWorkers; $i++) {
            if ($workers[$i] != 0) {
                $workers[$i] -= $minDuration;
                if ($workers[$i] == 0) {
                    finishStep($deps, $allSteps, $tasks[$i]);
                    $tasks[$i] = 0;
                }
            }
        }
        $time += $minDuration;
    }

    return $time;
}

function isBeingWorkedOn($step, $tasks)
{
    return in_array($step, $tasks);
}

function findMinDuration($durations)
{
    $min = PHP_INT_MAX;
    foreach ($durations as $duration) {
        if ($duration > 0 && $duration < $min) {
            $min = $duration;
        }
    }
    return $min;
}

function finishStep(&$deps, &$allSteps, $step)
{
    unset($allSteps[$step]);
    foreach ($deps as $s => &$dependencies) {
        if (($key = array_search($step, $dependencies)) !== false) {
            unset($dependencies[$key]);
        }
    }
}