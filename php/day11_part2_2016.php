<?php

function main() {
    $input = file_get_contents("./input.txt");
    $ans = rtgHellDay($input);
    echo $ans . "\n";
}

function rtgHellDay($input) {
    $currentState = newInitialState($input);
    array_push($currentState->floors[0], new Half(false, "elerium"), new Half(true, "elerium"), new Half(false, "dilithium"), new Half(true, "dilithium"));

    $queue = [$currentState];
    $prevStates = [];

    while (count($queue) > 0) {
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

class Half {
    public $isChip;
    public $material;

    function __construct($isChip, $material) {
        $this->isChip = $isChip;
        $this->material = $material;
    }

    function __toString() {
        return $this->material . ($this->isChip ? " microchip" : " generator");
    }
}

class State {
    public $floors;
    public $elevatorLevel;
    public $steps;

    function __construct() {
        $this->floors = [[], [], [], []];
        $this->elevatorLevel = 0;
        $this->steps = 0;
    }

    function hashKey() {
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
        foreach ($mapGenToIndex as $material => $genIndex) {
            $genChipPairs[] = [$genIndex, $mapChipToIndex[$material]];
        }

        usort($genChipPairs, function ($a, $b) {
            return $a[0] <=> $b[0] ?: $a[1] <=> $b[1];
        });

        return json_encode([$this->elevatorLevel, $genChipPairs]);
    }

    function isValid() {
        for ($i = 0; $i < count($this->floors); $i++) {
            $gensSeen = [];
            foreach ($this->floors[$i] as $half) {
                if (!$half->isChip) {
                    $gensSeen[$half->material] = true;
                }
            }

            if (count($gensSeen) == 0) {
                continue;
            }

            foreach ($this->floors[$i] as $half) {
                if ($half->isChip && !isset($gensSeen[$half->material])) {
                    return false;
                }
            }
        }

        return true;
    }

    function isDone() {
        $lenSum = 0;
        for ($i = 0; $i < 3; $i++) {
            $lenSum += count($this->floors[$i]);
        }
        return $lenSum == 0;
    }

    function clone() {
        $cl = new State();
        $cl->elevatorLevel = $this->elevatorLevel;
        $cl->steps = $this->steps;
        for ($i = 0; $i < 4; $i++) {
            $cl->floors[$i] = array_map(function ($item) { return clone $item; }, $this->floors[$i]);
        }
        return $cl;
    }

    function getNextStates() {
        $futureStates = [];
        $eleDiffs = [];
        if ($this->elevatorLevel < 3) {
            $eleDiffs[] = 1;
        }
        if ($this->elevatorLevel > 0) {
            $eleDiffs[] = -1;
        }

        foreach ($eleDiffs as $eleDiff) {
            for ($i = 0; $i < count($this->floors[$this->elevatorLevel]); $i++) {
                for ($j = $i + 1; $j < count($this->floors[$this->elevatorLevel]); $j++) {
                    $futureStates = array_merge($futureStates, $this->moveItems([$i, $j], $eleDiff));
                }
                $futureStates = array_merge($futureStates, $this->moveItems([$i], $eleDiff));
            }
        }

        return $futureStates;
    }

    private function moveItems($indices, $eleDiff) {
        $cl = $this->clone();
        $cl->elevatorLevel += $eleDiff;
        $cl->steps++;
        $oldLevel = $this->elevatorLevel;
        $newLevel = $cl->elevatorLevel;

        foreach ($indices as $index) {
            $cl->floors[$newLevel][] = $cl->floors[$oldLevel][$index];
        }

        foreach (array_reverse($indices) as $index) {
            array_splice($cl->floors[$oldLevel], $index, 1);
        }

        if ($cl->isValid()) {
            return [$cl];
        }
        return [];
    }
}

function newInitialState($input) {
    $s = new State();
    $lines = explode("\n", $input);
    foreach ($lines as $lineIndex => $line) {
        $parts = preg_split('/\s+/', preg_replace('/[,.]/', '', $line));

        for ($i = 0; $i < count($parts); $i++) {
            if ($parts[$i] == "generator") {
                $material = $parts[$i - 1];
                $s->floors[$lineIndex][] = new Half(false, $material);
            } elseif (strpos($parts[$i], "microchip") !== false) {
                $material = substr($parts[$i - 1], 0, strpos($parts[$i - 1], "-comp"));
                $s->floors[$lineIndex][] = new Half(true, $material);
            }
        }
    }

    return $s;
}

main();