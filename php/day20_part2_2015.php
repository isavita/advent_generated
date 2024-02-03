
<?php

$data = file_get_contents("input.txt");
$input = trim($data);
$target = intval($input);
$target /= 11;

$houses = array_fill(0, $target + 1, 0);

for ($elf = 1; $elf <= $target; $elf++) {
    for ($house = $elf; $house <= $elf * 50 && $house <= $target; $house += $elf) {
        $houses[$house] += $elf;
    }
}

foreach ($houses as $houseNumber => $presents) {
    if ($presents >= $target) {
        echo $houseNumber . "\n";
        break;
    }
}
?>
