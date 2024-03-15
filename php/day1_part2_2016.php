<?php

class Position {
    public $x, $y;

    public function __construct($x, $y) {
        $this->x = $x;
        $this->y = $y;
    }

    public function __toString() {
        return "(" . $this->x . ", " . $this->y . ")";
    }

    public function __hash() {
        return $this->x . "," . $this->y;
    }
}

function firstRevisitedDistance($instructions) {
    $pos = new Position(0, 0);
    $visited = array();
    $visited[$pos->__hash()] = true;
    $directions = array(
        new Position(0, 1),
        new Position(1, 0),
        new Position(0, -1),
        new Position(-1, 0)
    );
    $dirIndex = 0; // Start facing North

    foreach ($instructions as $instruction) {
        $turn = substr($instruction, 0, 1);
        $blocks = intval(substr($instruction, 1));

        if ($turn == 'R') {
            $dirIndex = ($dirIndex + 1) % 4;
        } else {
            $dirIndex = ($dirIndex - 1 + 4) % 4;
        }

        for ($i = 0; $i < $blocks; $i++) {
            $pos->x += $directions[$dirIndex]->x;
            $pos->y += $directions[$dirIndex]->y;

            $hash = $pos->__hash();
            if (isset($visited[$hash])) {
                return abs($pos->x) + abs($pos->y);
            }
            $visited[$hash] = true;
        }
    }

    return -1; // No location visited twice
}

$file = fopen('input.txt', 'r');
$instructions = array();
while (($line = fgets($file)) !== false) {
    $instructions = explode(', ', trim($line));
}
fclose($file);

echo firstRevisitedDistance($instructions) . "\n";