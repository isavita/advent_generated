<?php

function rtgHellDay($input) {
    $currentState = newInitialState($input);

    $queue = [$currentState];
    $prevStates = [];
    while (!empty($queue)) {
        $front = array_shift($queue);

        if ($front->isDone()) {
            return $front->steps;
        }

        $hash = $front->hashKey();
        if (isset($prevStates[$hash])) {
            continue;
        }
        $prevStates[$hash] = true;

        $nextStates = $front->getNextStates();
        $queue = array_merge($queue, $nextStates);
    }

    return -1;
}

function newInitialState($input) {
    $s = new State();

    $lines = explode("\n", $input);
    foreach ($lines as $lineIndex => $line) {
        $parts = array_map(function($v) {
            return trim($v, ",.");
        }, explode(" ", $line));

        foreach ($parts as $i => $word) {
            if ($word == "generator") {
                $material = $parts[$i - 1];
                $s->floors[$lineIndex][] = new Halves(false, $material);
            } elseif ($word == "microchip") {
                $material = substr($parts[$i - 1], 0, strpos($parts[$i - 1], "-comp"));
                $s->floors[$lineIndex][] = new Halves(true, $material);
            }
        }
    }

    return $s;
}

class Halves {
    public $isChip;
    public $material;

    public function __construct($isChip, $material) {
        $this->isChip = $isChip;
        $this->material = $material;
    }

    public function __toString() {
        return $this->material . ($this->isChip ? " microchip" : " generator");
    }
}

class State {
    public $floors = [[], [], [], []];
    public $elevatorLevel = 0;
    public $steps = 0;

    public function hashKey() {
        $mapGenToIndex = [];
        $mapChipToIndex = [];
        foreach ($this->floors as $flIndex => $fl) {
            foreach ($fl as $half) {
                if ($half->isChip) {
                    $mapChipToIndex[$half->material] = $flIndex;
                } else {
                    $mapGenToIndex[$half->material] = $flIndex;
                }
            }
        }

        $genChipPairs = [];
        foreach ($mapGenToIndex as $material => $index) {
            $genChipPairs[] = [$index, $mapChipToIndex[$material]];
        }

        usort($genChipPairs, function($a, $b) {
            return $a[0] <=> $b[0] ?: $a[1] <=> $b[1];
        });

        return $this->elevatorLevel . serialize($genChipPairs);
    }

    public function isValid() {
        foreach ($this->floors as $fl) {
            $gensSeen = [];
            foreach ($fl as $half) {
                if (!$half->isChip) {
                    $gensSeen[$half->material] = true;
                }
            }

            if (empty($gensSeen)) {
                continue;
            }

            foreach ($fl as $half) {
                if ($half->isChip && !isset($gensSeen[$half->material])) {
                    return false;
                }
            }
        }
        return true;
    }

    public function isDone() {
        $lenSum = 0;
        for ($i = 0; $i < 3; $i++) {
            $lenSum += count($this->floors[$i]);
        }
        return $lenSum === 0;
    }

    public function getMovablePermIndices() {
        $permsToMove = [];
        $currentLevel = $this->floors[$this->elevatorLevel];

        for ($i = 0; $i < count($currentLevel); $i++) {
            for ($j = $i + 1; $j < count($currentLevel); $j++) {
                $permsToMove[] = [$i, $j];
            }
            $permsToMove[] = [$i];
        }
        return $permsToMove;
    }

    public function clone() {
        $cl = new State();
        $cl->elevatorLevel = $this->elevatorLevel;
        $cl->steps = $this->steps;
        foreach ($this->floors as $i => $fl) {
            $cl->floors[$i] = $fl;
        }
        return $cl;
    }

    public function getNextStates() {
        $futureStates = [];
        $movablePermIndices = $this->getMovablePermIndices();
        $eleDiffs = [];
        if ($this->elevatorLevel < 3) {
            $eleDiffs[] = 1;
        }
        if ($this->elevatorLevel > 0) {
            $eleDiffs[] = -1;
        }

        foreach ($eleDiffs as $eleDiff) {
            foreach ($movablePermIndices as $permIndices) {
                $cl = $this->clone();
                $cl->elevatorLevel += $eleDiff;
                $cl->steps++;
                $oldLevel = $this->elevatorLevel;
                $newLevel = $cl->elevatorLevel;

                foreach ($permIndices as $index) {
                    $cl->floors[$newLevel][] = $cl->floors[$oldLevel][$index];
                }
                foreach (array_reverse($permIndices) as $index) {
                    array_splice($cl->floors[$oldLevel], $index, 1);
                }

                if ($cl->isValid()) {
                    $futureStates[] = $cl;
                }
            }
        }

        return $futureStates;
    }
}

$input = file_get_contents("input.txt");
$answer = rtgHellDay($input);
echo $answer;