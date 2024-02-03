
<?php

class Marble {
    public $value;
    public $prev;
    public $next;

    public function __construct($value) {
        $this->value = $value;
        $this->prev = null;
        $this->next = null;
    }
}

function readInput($filename) {
    $lines = file($filename);
    $parts = explode(" ", $lines[0]);
    $players = (int)$parts[0];
    $lastMarble = (int)$parts[6];
    return [$players, $lastMarble];
}

function playMarbleGame($players, $lastMarble) {
    $scores = array_fill(0, $players, 0);
    $current = new Marble(0);
    $current->next = $current;
    $current->prev = $current;

    for ($marble = 1; $marble <= $lastMarble; $marble++) {
        if ($marble % 23 == 0) {
            $player = $marble % $players;
            for ($i = 0; $i < 7; $i++) {
                $current = $current->prev;
            }
            $scores[$player] += $marble + $current->value;
            $current->prev->next = $current->next;
            $current->next->prev = $current->prev;
            $current = $current->next;
        } else {
            $current = $current->next;
            $newMarble = new Marble($marble);
            $newMarble->prev = $current;
            $newMarble->next = $current->next;
            $current->next->prev = $newMarble;
            $current->next = $newMarble;
            $current = $newMarble;
        }
    }

    $maxScore = 0;
    foreach ($scores as $score) {
        if ($score > $maxScore) {
            $maxScore = $score;
        }
    }
    return $maxScore;
}

list($players, $lastMarble) = readInput("input.txt");
echo playMarbleGame($players, $lastMarble) . PHP_EOL;
?>
