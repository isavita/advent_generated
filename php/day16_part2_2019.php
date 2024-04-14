<?php

$input = trim(file_get_contents("input.txt"));
$inputLength = strlen($input);
$repeatedTimes = 10000;

// Calculate the offset
$offset = (int)substr($input, 0, 7);

// Since we only need the part after the offset, create a reduced array
$reducedInput = [];
for ($i = 0; $i < $repeatedTimes; $i++) {
    for ($j = 0; $j < $inputLength; $j++) {
        if ($i * $inputLength + $j >= $offset) {
            $reducedInput[] = (int)$input[$j];
        }
    }
}

// Apply FFT for 100 phases starting from the offset
for ($phase = 0; $phase < 100; $phase++) {
    $sum = 0;
    for ($i = count($reducedInput) - 1; $i >= 0; $i--) {
        $sum = ($sum + $reducedInput[$i]) % 10;
        $reducedInput[$i] = $sum;
    }
}

// Print the eight-digit message
for ($i = 0; $i < 8; $i++) {
    echo $reducedInput[$i];
}
echo "\n";

?>