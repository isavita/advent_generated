<?php
$input = file_get_contents("input.txt");
$digits = array_map('intval', str_split(trim($input)));

for ($phase = 0; $phase < 100; $phase++) {
    $digits = applyFFT($digits);
}

echo implode('', array_slice($digits, 0, 8)) . "\n";

function applyFFT($input) {
    $basePattern = [0, 1, 0, -1];
    $output = [];
    $length = count($input);
    for ($i = 0; $i < $length; $i++) {
        $sum = 0;
        foreach ($input as $j => $val) {
            $patternIndex = intval(floor(($j + 1) / ($i + 1))) % 4;
            $patternValue = $basePattern[$patternIndex];
            $sum += $val * $patternValue;
        }
        $output[$i] = abs($sum) % 10; // Use the built-in abs() function directly
    }
    return $output;
}
?>