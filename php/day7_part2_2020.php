<?php
$file = fopen("input.txt", "r") or die("Unable to open file");

$rules = array();
$ruleRegex = "/(\d+) (\w+ \w+) bags?[,.]*/";

while(!feof($file)) {
    $line = fgets($file);
    $parts = explode(" bags contain ", $line);
    $container = $parts[0];
    $contents = $parts[1];

    if ($contents == "no other bags.") {
        continue;
    }

    preg_match_all($ruleRegex, $contents, $matches, PREG_SET_ORDER);
    foreach ($matches as $match) {
        $count = intval($match[1]);
        $rules[$container][] = array("color" => $match[2], "count" => $count);
    }
}
fclose($file);

$totalBags = countBags("shiny gold", $rules) - 1;
echo $totalBags;

function countBags($color, $rules) {
    $count = 1;
    if (array_key_exists($color, $rules)) {
        foreach ($rules[$color] as $rule) {
            $count += $rule["count"] * countBags($rule["color"], $rules);
        }
    }
    return $count;
}
?>