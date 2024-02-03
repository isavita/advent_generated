
<?php

$input = file_get_contents("input.txt");
$numbers = array_map('intval', explode(" ", $input));

function parseTree($data, $index) {
    $childCount = $data[$index];
    $metaCount = $data[$index + 1];
    $index += 2;

    $sum = 0;
    for ($i = 0; $i < $childCount; $i++) {
        list($childSum, $newIndex) = parseTree($data, $index);
        $sum += $childSum;
        $index = $newIndex;
    }

    for ($i = 0; $i < $metaCount; $i++) {
        $sum += $data[$index + $i];
    }
    $index += $metaCount;

    return array($sum, $index);
}

list($sum, $_) = parseTree($numbers, 0);
echo $sum . "\n";

?>
