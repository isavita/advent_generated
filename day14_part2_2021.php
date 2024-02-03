
<?php

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);
$template = $lines[0];
$rules = [];
for ($i = 1; $i < count($lines); $i++) {
    $line = $lines[$i];
    if ($line == "") {
        continue;
    }
    $parts = explode(" -> ", $line);
    $rules[$parts[0]] = $parts[1];
}

$pairCounts = [];
for ($i = 0; $i < strlen($template) - 1; $i++) {
    $pair = substr($template, $i, 2);
    if (!array_key_exists($pair, $pairCounts)) {
        $pairCounts[$pair] = 0;
    }
    $pairCounts[$pair]++;
}

for ($step = 0; $step < 40; $step++) {
    $newPairCounts = [];
    foreach ($pairCounts as $pair => $count) {
        if (array_key_exists($pair, $rules)) {
            $insert = $rules[$pair];
            $newPairCounts[$pair[0].$insert] = isset($newPairCounts[$pair[0].$insert]) ? $newPairCounts[$pair[0].$insert] + $count : $count;
            $newPairCounts[$insert.$pair[1]] = isset($newPairCounts[$insert.$pair[1]]) ? $newPairCounts[$insert.$pair[1]] + $count : $count;
        } else {
            $newPairCounts[$pair] = isset($newPairCounts[$pair]) ? $newPairCounts[$pair] + $count : $count;
        }
    }
    $pairCounts = $newPairCounts;
}

$elementCounts = [];
foreach ($pairCounts as $pair => $count) {
    $elementCounts[$pair[0]] = isset($elementCounts[$pair[0]]) ? $elementCounts[$pair[0]] + $count : $count;
}
$elementCounts[$template[strlen($template) - 1]]++;

$maxCount = 0;
$minCount = PHP_INT_MAX;
foreach ($elementCounts as $count) {
    $maxCount = max($maxCount, $count);
    $minCount = min($minCount, $count);
}

echo $maxCount - $minCount . "\n";
?>
