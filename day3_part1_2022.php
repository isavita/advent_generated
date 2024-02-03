
<?php

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);

$sum = 0;

foreach ($lines as $line) {
    $half = strlen($line) / 2;
    $firstCompartment = substr($line, 0, $half);
    $secondCompartment = substr($line, $half);

    $compartmentMap = array_count_values(str_split($firstCompartment));

    foreach (str_split($secondCompartment) as $item) {
        if (array_key_exists($item, $compartmentMap)) {
            $sum += itemPriority($item);
            break;
        }
    }
}

echo $sum;

function itemPriority($item) {
    if (ctype_lower($item)) {
        return ord($item) - ord('a') + 1;
    }
    return ord($item) - ord('A') + 27;
}
?>
