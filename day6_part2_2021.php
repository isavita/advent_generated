
<?php

$file = fopen("input.txt", "r");
if ($file) {
    $lanternFishCounts = array_fill(0, 9, 0);

    while (($line = fgets($file)) !== false) {
        $fishAges = explode(",", $line);
        foreach ($fishAges as $age) {
            $ageCount = intval($age);
            $lanternFishCounts[$ageCount]++;
        }
    }

    for ($i = 0; $i < 256; $i++) {
        $newLanternFish = $lanternFishCounts[0];
        for ($j = 0; $j < count($lanternFishCounts) - 1; $j++) {
            $lanternFishCounts[$j] = $lanternFishCounts[$j + 1];
        }
        $lanternFishCounts[6] += $newLanternFish;
        $lanternFishCounts[8] = $newLanternFish;
    }

    echo array_sum($lanternFishCounts) . "\n";

    fclose($file);
} else {
    echo "Error opening the file.\n";
}
?>
