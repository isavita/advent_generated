<?php
class Deck {
    private $cards = [];

    public function copy(int $n): Deck {
        $copy = new Deck();
        for ($i = 0; $i < $n; $i++) {
            $copy->cards[$i] = $this->cards[$i];
        }
        return $copy;
    }

    public function score(): int {
        $score = 0;
        for ($i = 0; $i < count($this->cards); $i++) {
            $score += $this->cards[$i] * (count($this->cards) - $i);
        }
        return $score;
    }

    public function addCard(int $card): void {
        $this->cards[] = $card;
    }

    public function getCard(int $index): int {
        return $this->cards[$index];
    }

    public function removeCard(): int {
        return array_shift($this->cards);
    }

    public function isEmpty(): bool {
        return empty($this->cards);
    }

    public function count(): int {
        return count($this->cards);
    }
}

function playRecursiveCombat(Deck $player1, Deck $player2): array {
    $previousRounds = [];
    while (!$player1->isEmpty() && !$player2->isEmpty()) {
        $roundKey = serialize([$player1, $player2]);
        if (in_array($roundKey, $previousRounds, true)) {
            return [$player1, new Deck()];
        }
        $previousRounds[] = $roundKey;

        $card1 = $player1->removeCard();
        $card2 = $player2->removeCard();

        if ($player1->count() >= $card1 && $player2->count() >= $card2) {
            list($subPlayer1, $subPlayer2) = playRecursiveCombat($player1->copy($card1), $player2->copy($card2));
            if (!$subPlayer1->isEmpty()) {
                $player1->addCard($card1);
                $player1->addCard($card2);
            } else {
                $player2->addCard($card2);
                $player2->addCard($card1);
            }
        } else {
            if ($card1 > $card2) {
                $player1->addCard($card1);
                $player1->addCard($card2);
            } else {
                $player2->addCard($card2);
                $player2->addCard($card1);
            }
        }
    }
    return [$player1, $player2];
}

$file = fopen("input.txt", "r");
$player1Deck = new Deck();
$player2Deck = new Deck();
$currentDeck = &$player1Deck;
while (($line = fgets($file)) !== false) {
    $line = trim($line);
    if ($line === "") {
        $currentDeck = &$player2Deck;
        continue;
    }
    if (strpos($line, "Player") !== false) {
        continue;
    }
    $currentDeck->addCard((int)$line);
}
fclose($file);

list($player1Deck, $player2Deck) = playRecursiveCombat($player1Deck, $player2Deck);
$winningDeck = $player1Deck->isEmpty() ? $player2Deck : $player1Deck;
echo $winningDeck->score() . PHP_EOL;