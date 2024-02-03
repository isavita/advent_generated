
<?php

$input = file_get_contents("input.txt");
$input = trim($input);
$lines = explode("\n", $input);

class Card {
    public $winnings = [];
    public $givens = [];
    public $totalCount = 1;
}

function getPointsForCard($card) {
    $points = 0;
    foreach ($card->givens as $given => $count) {
        if (isset($card->winnings[$given])) {
            $points += $count * $card->winnings[$given];
        }
    }
    return $points;
}

function lexLineIntoCard($line) {
    list(, $cardDataStr) = explode(": ", $line);
    $cardData = explode(" | ", $cardDataStr);

    $winnings = [];
    preg_match_all("/[0-9]{1,2}/", $cardData[0], $matches);
    foreach ($matches[0] as $point) {
        if (isset($winnings[$point])) {
            $winnings[$point]++;
        } else {
            $winnings[$point] = 1;
        }
    }

    $givens = [];
    preg_match_all("/[0-9]{1,2}/", $cardData[1], $matches);
    foreach ($matches[0] as $point) {
        if (isset($givens[$point])) {
            $givens[$point]++;
        } else {
            $givens[$point] = 1;
        }
    }

    $card = new Card();
    $card->winnings = $winnings;
    $card->givens = $givens;

    return $card;
}

$cards = [];
foreach ($lines as $line) {
    if (empty($line)) {
        continue;
    }
    $card = lexLineIntoCard($line);
    $cards[] = $card;
}

foreach ($cards as $i => $card) {
    $points = getPointsForCard($card);

    for ($j = 1; $j <= $points; $j++) {
        if (isset($cards[$i + $j])) {
            $cards[$i + $j]->totalCount += 1 * $card->totalCount;
        }
    }
}

$totalCards = 0;
foreach ($cards as $card) {
    $totalCards += $card->totalCount;
}

echo $totalCards . PHP_EOL;
?>
