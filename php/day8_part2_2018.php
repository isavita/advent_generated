
<?php

$input = file_get_contents("input.txt");
$numbers = array_map('intval', explode(" ", $input));

function parseTree($data, $index) {
    $childCount = $data[$index];
    $metaCount = $data[$index + 1];
    $index += 2;

    $childValues = [];
    for ($i = 0; $i < $childCount; $i++) {
        list($childValue, $index) = parseTree($data, $index);
        $childValues[] = $childValue;
    }

    $value = 0;
    if ($childCount == 0) {
        for ($i = 0; $i < $metaCount; $i++) {
            $value += $data[$index + $i];
        }
    } else {
        for ($i = 0; $i < $metaCount; $i++) {
            $metadata = $data[$index + $i];
            if ($metadata <= $childCount && $metadata > 0) {
                $value += $childValues[$metadata - 1];
            }
        }
    }
    $index += $metaCount;

    return [$value, $index];
}

list($value, $_) = parseTree($numbers, 0);
echo $value . PHP_EOL;
?>
