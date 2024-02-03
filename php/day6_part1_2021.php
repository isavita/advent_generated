
<?php

$file = fopen("input.txt", "r");
$input = fgets($file);
fclose($file);

$fishes = array_fill(0, 9, 0);
$fishStrs = explode(",", $input);
foreach ($fishStrs as $fishStr) {
    $fish = intval($fishStr);
    $fishes[$fish]++;
}

for ($day = 1; $day <= 80; $day++) {
    $newFish = $fishes[0];
    for ($i = 1; $i < count($fishes); $i++) {
        $fishes[$i - 1] = $fishes[$i];
    }
    $fishes[6] += $newFish;
    $fishes[8] = $newFish;
}

$totalFish = array_sum($fishes);
echo $totalFish . "\n";
?>
