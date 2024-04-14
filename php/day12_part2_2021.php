<?php

function main() {
    $input = trim(file_get_contents("input.txt"));
    $answer = solve($input);
    echo $answer . PHP_EOL;
}

function solve($input) {
    $parsed = parseInput($input);
    $graph = [];

    foreach ($parsed as $pair) {
        if (!isset($graph[$pair[0]])) {
            $graph[$pair[0]] = [];
        }
        if (!isset($graph[$pair[1]])) {
            $graph[$pair[1]] = [];
        }
        $graph[$pair[0]][$pair[1]] = true;
        $graph[$pair[1]][$pair[0]] = true;
    }

    return walk($graph, "start", ["start" => 1], false);
}

function walk($graph, $current, $visited, $doubleUsed) {
    if ($current === "end") {
        return 1;
    }

    $pathsToEnd = 0;
    foreach (array_keys($graph[$current]) as $visitable) {
        if ($visitable === "start") {
            continue;
        }

        $canVisit = true;
        $newDoubleUsed = $doubleUsed;
        if (ctype_lower($visitable) && isset($visited[$visitable]) && $visited[$visitable] > 0) {
            if ($doubleUsed) {
                $canVisit = false;
            } else {
                $newDoubleUsed = true; // Update the usage of double visit
            }
        }

        if ($canVisit) {
            $visited[$visitable] = ($visited[$visitable] ?? 0) + 1;
            $pathsToEnd += walk($graph, $visitable, $visited, $newDoubleUsed);
            $visited[$visitable]--;
        }
    }

    return $pathsToEnd;
}

function parseInput($input) {
    $lines = explode("\n", $input);
    $ans = [];
    foreach ($lines as $line) {
        $ans[] = explode("-", $line);
    }
    return $ans;
}

main();

?>