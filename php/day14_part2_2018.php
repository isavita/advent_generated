
<?php
$input = trim(file_get_contents("input.txt"));
$scoreboard = "37";
$elf1 = 0;
$elf2 = 1;
$inputLen = strlen($input);

while (true) {
    $newScore = (int)$scoreboard[$elf1] + (int)$scoreboard[$elf2];
    $newScoreStr = (string)$newScore;
    for ($i = 0; $i < strlen($newScoreStr); $i++) {
        $scoreboard .= $newScoreStr[$i];
        if (str_ends_with($scoreboard, $input)) {
            echo strlen($scoreboard) - $inputLen . "\n";
            return;
        }
    }
    $elf1 = ($elf1 + (int)$scoreboard[$elf1] + 1) % strlen($scoreboard);
    $elf2 = ($elf2 + (int)$scoreboard[$elf2] + 1) % strlen($scoreboard);
}
?>
