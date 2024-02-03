
<?php

$file = fopen("input.txt", "r") or die("Unable to open file!");
$caloriesList = [];
$currentCalories = 0;

while (!feof($file)) {
    $line = trim(fgets($file));

    if ($line == "") {
        $caloriesList[] = $currentCalories;
        $currentCalories = 0;
        continue;
    }

    $calories = intval($line);
    $currentCalories += $calories;
}

$caloriesList[] = $currentCalories;
rsort($caloriesList);

$topThreeSum = 0;
for ($i = 0; $i < 3 && $i < count($caloriesList); $i++) {
    $topThreeSum += $caloriesList[$i];
}

fclose($file);

echo $topThreeSum . PHP_EOL;
?>
