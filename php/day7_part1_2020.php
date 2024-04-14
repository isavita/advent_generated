<?php

function countCanContain($target, $contains) {
    $seen = [];
    $dfs = function($bag) use ($contains, &$dfs, &$seen) {
        if (isset($contains[$bag])) {
            foreach ($contains[$bag] as $outer) {
                if (!isset($seen[$outer])) {
                    $seen[$outer] = true;
                    $dfs($outer);
                }
            }
        }
    };
    $dfs($target);
    return count($seen);
}

$file = fopen("input.txt", "r");
if (!$file) {
    echo "Error opening file\n";
    exit;
}

$contains = [];

while (($line = fgets($file)) !== false) {
    $parts = explode(" bags contain ", trim($line));
    $container = $parts[0];
    if ($parts[1] == "no other bags.") {
        continue;
    }
    $containedBags = explode(", ", $parts[1]);
    foreach ($containedBags as $bag) {
        $words = explode(" ", $bag);
        $bagName = $words[1] . " " . $words[2];
        $contains[$bagName][] = $container;
    }
}

fclose($file);
$count = countCanContain("shiny gold", $contains);
echo $count . "\n";

?>