
<?php

$input = file_get_contents("input.txt");
$values = explode("\n", $input);

function filterValues($values, $criteria) {
    for ($i = 0; $i < strlen($values[0]); $i++) {
        $zeros = 0;
        $ones = 0;
        foreach ($values as $val) {
            if ($val[$i] == '0') {
                $zeros++;
            } else {
                $ones++;
            }
        }
        $keep = $criteria($zeros, $ones);
        $values = filterByBit($values, $i, $keep);
        if (count($values) == 1) {
            break;
        }
    }
    return $values[0];
}

function filterByBit($values, $bitIndex, $keep) {
    $filtered = [];
    foreach ($values as $val) {
        if ($val[$bitIndex] == $keep) {
            $filtered[] = $val;
        }
    }
    return $filtered;
}

$oxygenGeneratorRating = filterValues($values, function($zeros, $ones) {
    if ($zeros > $ones) {
        return '0';
    } else {
        return '1';
    }
});
$oxygenGeneratorRatingInt = bindec($oxygenGeneratorRating);

$co2ScrubberRating = filterValues($values, function($zeros, $ones) {
    if ($zeros <= $ones) {
        return '0';
    } else {
        return '1';
    }
});
$co2ScrubberRatingInt = bindec($co2ScrubberRating);

echo $oxygenGeneratorRatingInt * $co2ScrubberRatingInt . "\n";
?>
