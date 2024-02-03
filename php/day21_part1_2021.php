
<?php

$data = file_get_contents("input.txt");
$lines = explode("\n", $data);
$player1Start = intval(trim(substr($lines[0], 28)));
$player2Start = intval(trim(substr($lines[1], 28)));
$player1Pos = $player1Start;
$player2Pos = $player2Start;
$player1Score = 0;
$player2Score = 0;
$dieRoll = 1;
$rollCount = 0;

while (true) {
    // Player 1
    $rolls = $dieRoll % 100 + ($dieRoll + 1) % 100 + ($dieRoll + 2) % 100;
    $rollCount += 3;
    $dieRoll += 3;
    $player1Pos = ($player1Pos + $rolls - 1) % 10 + 1;
    $player1Score += $player1Pos;

    if ($player1Score >= 1000) {
        echo "Result: " . $player2Score * $rollCount . "\n";
        break;
    }

    // Player 2
    $rolls = $dieRoll % 100 + ($dieRoll + 1) % 100 + ($dieRoll + 2) % 100;
    $rollCount += 3;
    $dieRoll += 3;
    $player2Pos = ($player2Pos + $rolls - 1) % 10 + 1;
    $player2Score += $player2Pos;

    if ($player2Score >= 1000) {
        echo $player1Score * $rollCount . "\n";
        break;
    }
}
?>
