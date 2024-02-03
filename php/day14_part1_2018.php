
<?php

$input = intval(file_get_contents("input.txt"));

$scoreboard = [3, 7];
$elf1 = 0;
$elf2 = 1;

while (count($scoreboard) < $input + 10) {
    $newScore = $scoreboard[$elf1] + $scoreboard[$elf2];
    if ($newScore >= 10) {
        $scoreboard[] = intval($newScore / 10);
    }
    $scoreboard[] = $newScore % 10;

    $elf1 = ($elf1 + $scoreboard[$elf1] + 1) % count($scoreboard);
    $elf2 = ($elf2 + $scoreboard[$elf2] + 1) % count($scoreboard);
}

for ($i = $input; $i < $input + 10; $i++) {
    echo $scoreboard[$i];
}
echo "\n";
?>
