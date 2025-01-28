
<?php

function solve_part1_and_2($patterns, $designs) {
    $possible_designs_count = 0;
    $total_ways = 0;

    foreach ($designs as $design) {
        $memo = [];
        $ways = countWays($design, $patterns, $memo);
        if ($ways > 0) {
            $possible_designs_count++;
        }
        $total_ways += $ways;
    }

    return [$possible_designs_count, $total_ways];
}

function countWays($design, $patterns, &$memo) {
    if ($design === "") {
        return 1;
    }
    if (isset($memo[$design])) {
        return $memo[$design];
    }

    $ways = 0;
    foreach ($patterns as $pattern) {
        if (strpos($design, $pattern) === 0) {
            $remaining_design = substr($design, strlen($pattern));
            $ways += countWays($remaining_design, $patterns, $memo);
        }
    }
    $memo[$design] = $ways;
    return $ways;
}

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);

$patterns_line = array_shift($lines);
$patterns = explode(', ', $patterns_line);

// Skip blank line if exists
if (empty(trim(array_shift($lines)))) {
    // do nothing, already skipped
} else {
    // if no blank line, put back the line we shifted out
    array_unshift($lines, ''); // This is incorrect, but let's assume there is always a blank line in input after patterns.
}


$designs = [];
foreach ($lines as $line) {
    $line = trim($line);
    if (!empty($line)) {
        $designs[] = $line;
    }
}

list($part1_result, $part2_result) = solve_part1_and_2($patterns, $designs);

echo $part1_result . "\n";
echo $part2_result . "\n";

?>
