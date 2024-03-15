<?php
$filename = "input.txt";

// Read and parse the input file
list($deps, $allSteps) = parseInput($filename);
// Compute the order of steps
$order = topologicalSort($deps, $allSteps);
echo $order . "\n";

function parseInput($filename) {
    $deps = array();
    $allSteps = array();

    $file = fopen($filename, "r");
    if ($file) {
        while (($line = fgets($file)) !== false) {
            preg_match("/Step (.) must be finished before step (.) can begin./", $line, $matches);
            $a = $matches[1];
            $b = $matches[2];
            if (!isset($deps[$b])) {
                $deps[$b] = array();
            }
            $deps[$b][] = $a;
            $allSteps[$a] = true;
            $allSteps[$b] = true;
        }
        fclose($file);
    } else {
        die("Unable to open file: " . $filename);
    }

    return array($deps, $allSteps);
}

function topologicalSort($deps, $allSteps) {
    $order = "";
    $available = array();

    // Find initial available steps (with no dependencies)
    foreach ($allSteps as $step => $value) {
        if (!isset($deps[$step]) || count($deps[$step]) == 0) {
            $available[] = $step;
        }
    }
    sort($available);

    while (!empty($available)) {
        // Pick the step that comes first alphabetically
        $next = array_shift($available);
        $order .= $next;

        // Check and update the availability of the next steps
        foreach ($deps as $step => $dependencies) {
            if (in_array($next, $dependencies)) {
                $deps[$step] = array_diff($dependencies, array($next));
                if (empty($deps[$step])) {
                    $available[] = $step;
                    sort($available);
                }
            }
        }
    }

    return $order;
}