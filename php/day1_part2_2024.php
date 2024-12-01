
<?php

// Part 1

function calculate_distance($left, $right) {
    sort($left);
    sort($right);
    $total_distance = 0;
    for ($i = 0; $i < count($left); $i++) {
        $total_distance += abs($left[$i] - $right[$i]);
    }
    return $total_distance;
}


$left = [];
$right = [];

$file = fopen("input.txt", "r");
if ($file) {
    while (($line = fgets($file)) !== false) {
        list($l, $r) = explode("   ", trim($line));
        $left[] = (int)$l;
        $right[] = (int)$r;
    }
    fclose($file);
} else {
    die("Unable to open file!");
}

$distance = calculate_distance($left, $right);
echo "Part 1: " . $distance . PHP_EOL;


//Part 2

function calculate_similarity($left, $right) {
    $right_counts = array_count_values($right);
    $total_similarity = 0;
    foreach ($left as $num) {
        $total_similarity += $num * ($right_counts[$num] ?? 0);
    }
    return $total_similarity;
}

$similarity = calculate_similarity($left, $right);
echo "Part 2: " . $similarity . PHP_EOL;

?>
