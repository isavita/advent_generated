
<?php

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);
$sum = 0;
$groupLineCounter = 0;
$groupItems = [];

foreach ($lines as $line) {
    $itemsMap = [];
    foreach (str_split($line) as $item) {
        if (!array_key_exists($item, $itemsMap)) {
            $itemsMap[$item] = 1;
        } else {
            $itemsMap[$item]++;
        }
    }
    $groupItems[$groupLineCounter] = $itemsMap;
    $groupLineCounter++;

    if ($groupLineCounter == 3) {
        $commonItems = [];
        foreach ($groupItems[0] as $item => $count) {
            if (isset($groupItems[1][$item]) && isset($groupItems[2][$item])) {
                $commonItems[$item] = 1;
            }
        }
        foreach ($commonItems as $item => $value) {
            $sum += itemPriority($item);
            break; // Since we need only one common item per group
        }
        $groupLineCounter = 0;
    }
}

echo $sum;

function itemPriority($item) {
    if (ctype_lower($item)) {
        return ord($item) - ord('a') + 1;
    }
    return ord($item) - ord('A') + 27;
}
