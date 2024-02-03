
<?php

$file = fopen("input.txt", "r") or die("Unable to open file!");
$initialState = "";
$rules = [];

while (!feof($file)) {
    $line = trim(fgets($file));
    if (strpos($line, "initial state") !== false) {
        $initialState = explode(": ", $line)[1];
    } elseif (strpos($line, "=>") !== false) {
        $parts = explode(" => ", $line);
        $rules[$parts[0]] = $parts[1][0];
    }
}

$state = [];
for ($i = 0; $i < strlen($initialState); $i++) {
    if ($initialState[$i] === '#') {
        $state[$i] = '#';
    }
}

for ($generation = 0; $generation < 20; $generation++) {
    $newState = [];
    $minPot = min(array_keys($state)) - 2;
    $maxPot = max(array_keys($state)) + 2;
    for ($i = $minPot; $i <= $maxPot; $i++) {
        $pattern = "";
        for ($j = $i - 2; $j <= $i + 2; $j++) {
            $pattern .= isset($state[$j]) && $state[$j] === '#' ? '#' : '.';
        }
        if ($rules[$pattern] === '#') {
            $newState[$i] = '#';
        }
    }
    $state = $newState;
}

$sum = 0;
foreach ($state as $k => $v) {
    $sum += $k;
}

echo $sum . PHP_EOL;

fclose($file);
?>
