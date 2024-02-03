
<?php

$file = fopen("input.txt", "r") or die("Unable to open file!");
$maxCalories = 0;
$currentCalories = 0;

while (!feof($file)) {
    $line = trim(fgets($file));

    if ($line == "") {
        if ($currentCalories > $maxCalories) {
            $maxCalories = $currentCalories;
        }
        $currentCalories = 0;
        continue;
    }

    $calories = intval($line);
    $currentCalories += $calories;
}

if ($currentCalories > $maxCalories) {
    $maxCalories = $currentCalories;
}

fclose($file);

echo $maxCalories . "\n";
?>
