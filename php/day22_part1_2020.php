
<?php

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);

$player1Deck = [];
$player2Deck = [];
$currentDeck = &$player1Deck;

foreach ($lines as $line) {
    if ($line == "") {
        $currentDeck = &$player2Deck;
        continue;
    }
    if (strpos($line, "Player") !== false) {
        continue;
    }
    $card = intval($line);
    $currentDeck[] = $card;
}

while (count($player1Deck) > 0 && count($player2Deck) > 0) {
    $card1 = array_shift($player1Deck);
    $card2 = array_shift($player2Deck);
    if ($card1 > $card2) {
        array_push($player1Deck, $card1, $card2);
    } else {
        array_push($player2Deck, $card2, $card1);
    }
}

$winningDeck = count($player1Deck) > 0 ? $player1Deck : $player2Deck;

$score = 0;
$deckSize = count($winningDeck);
foreach ($winningDeck as $i => $card) {
    $score += $card * ($deckSize - $i);
}

echo $score . PHP_EOL;
?>
