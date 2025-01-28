
<?php

class MinHeap {
    private $heap;

    public function __construct() {
        $this->heap = new Heap(function ($val1, $val2) {
            return $val1 < $val2;
        });
    }

    public function add($newNode) {
        $this->heap->add($newNode);
    }

    public function remove() {
        return $this->heap->remove();
    }

    public function length() {
        return $this->heap->length();
    }
}

class Heap {
    public $nodes = [];
    private $closerToRoot;

    public function __construct($closerToRoot) {
        $this->closerToRoot = $closerToRoot;
    }

    public function front() {
        return empty($this->nodes) ? null : $this->nodes[0];
    }

    public function add($newNode) {
        $this->nodes[] = $newNode;
        $this->heapifyFromEnd();
    }

    public function remove() {
        if (empty($this->nodes)) {
            return null;
        }

        $rootNode = $this->nodes[0];
        $this->nodes[0] = array_pop($this->nodes);
        $this->heapifyFromStart();
        return $rootNode;
    }

    public function length() {
        return count($this->nodes);
    }

    private function swap($i, $j) {
        [$this->nodes[$i], $this->nodes[$j]] = [$this->nodes[$j], $this->nodes[$i]];
    }

    private function heapifyFromEnd() {
        $currentIndex = count($this->nodes) - 1;
        while ($currentIndex > 0) {
            $parentIndex = (int)(($currentIndex - 1) / 2);
            $parentNode = $this->nodes[$parentIndex];
            if (($this->closerToRoot)($this->nodes[$currentIndex]->value(), $parentNode->value())) {
                $this->swap($parentIndex, $currentIndex);
                $currentIndex = $parentIndex;
            } else {
                break;
            }
        }
    }

    private function heapifyFromStart() {
        $currentIndex = 0;
        while (true) {
            $smallerChildIndex = $currentIndex;
            for ($i = 1; $i <= 2; $i++) {
                $childIndex = $currentIndex * 2 + $i;
                if ($childIndex < count($this->nodes) &&
                    ($this->closerToRoot)($this->nodes[$childIndex]->value(), $this->nodes[$smallerChildIndex]->value())) {
                    $smallerChildIndex = $childIndex;
                }
            }
            if ($smallerChildIndex === $currentIndex) {
                return;
            }
            $this->swap($smallerChildIndex, $currentIndex);
            $currentIndex = $smallerChildIndex;
        }
    }
}

$roomCoordToWantChar = [
    '[2,3]' => "A", '[3,3]' => "A",
    '[2,5]' => "B", '[3,5]' => "B",
    '[2,7]' => "C", '[3,7]' => "C",
    '[2,9]' => "D", '[3,9]' => "D",
];

function amphipod($input) {
    global $roomCoordToWantChar;
    $start = parseInput($input);
    $minHeap = new MinHeap();

    $minHeap->add($start);
    $seenGrids = [];

    while ($minHeap->length() > 0) {
        $front = $minHeap->remove();
        $key = json_encode($front->grid);
        
        if (isset($seenGrids[$key])) {
            continue;
        }
        $seenGrids[$key] = true;

        if ($front->allDone($roomCoordToWantChar)) {
            return $front->energyUsed;
        }
        
        $unsettledCoords = $front->getUnsettledCoords($roomCoordToWantChar);
        foreach ($unsettledCoords as $unsettledCoord) {
            [$ur, $uc] = $unsettledCoord;
            $nextMoves = $front->getNextPossibleMoves($unsettledCoord, $roomCoordToWantChar);
            
            foreach ($nextMoves as $nextCoord) {
                [$nr, $nc] = $nextCoord;
                if ($front->grid[$nr][$nc] !== ".") {
                    continue;
                }
                $cp = $front->copy();
                $cp->energyUsed += calcEnergy($cp->grid[$ur][$uc], $unsettledCoord, $nextCoord);
                $cp->grid[$nr][$nc] = $cp->grid[$ur][$uc];
                $cp->grid[$ur][$uc] = ".";

                $minHeap->add($cp);
            }
        }
    }
    return -1;
}

class State {
    public $grid;
    public $energyUsed;

    public function __construct($grid, $energyUsed = 0) {
        $this->grid = $grid;
        $this->energyUsed = $energyUsed;
    }

    public function value() {
        return $this->energyUsed;
    }

    public function copy() {
        $grid_copy = [];
        foreach($this->grid as $row) {
            $grid_copy[] = $row;
        }
        return new State($grid_copy, $this->energyUsed);
    }

    public function allDone($roomCoordToWantChar) {
        foreach ($roomCoordToWantChar as $coord => $want) {
            $coord_arr = json_decode($coord, true);
            if ($this->grid[$coord_arr[0]][$coord_arr[1]] !== $want) {
                return false;
            }
        }
        return true;
    }

    public function getUnsettledCoords($roomCoordToWantChar) {
        $unsettled = [];
        for ($col = 1; $col < count($this->grid[0]); $col++) {
            if (strpos("ABCD", $this->grid[1][$col]) !== false) {
                $unsettled[] = [1, $col];
            }
        }

        foreach ([3, 5, 7, 9] as $col) {
            $roomFullFromBack = true;
            for ($row = count($this->grid) - 2; $row >= 2; $row--) {
                $coord = [$row, $col];
                $coord_key = json_encode($coord);
                $wantChar = $roomCoordToWantChar[$coord_key];
                $gotChar = $this->grid[$row][$col];
                if ($gotChar !== ".") {
                    if ($gotChar !== $wantChar) {
                        $roomFullFromBack = false;
                        $unsettled[] = $coord;
                    } elseif ($gotChar === $wantChar && !$roomFullFromBack) {
                        $unsettled[] = $coord;
                    }
                }
            }
        }
        return $unsettled;
    }

    public function getNextPossibleMoves($unsettledCoord, $roomCoordToWantChar) {
        $unsettledChar = $this->grid[$unsettledCoord[0]][$unsettledCoord[1]];
        if (strpos("ABCD", $unsettledChar) === false) {
             return [];
        }

        $possible = [];
        $startedInHallway = $unsettledCoord[0] === 1;

        $queue = [$unsettledCoord];
        $seen = [];
        
        $coordsInFrontOfRooms = [
            '[1,3]' => true,
            '[1,5]' => true,
            '[1,7]' => true,
            '[1,9]' => true,
        ];
        
        while (!empty($queue)) {
            $front = array_shift($queue);

            $front_key = json_encode($front);
            if (isset($seen[$front_key])) {
                continue;
            }
            
            $seen[$front_key] = true;

            if ($front !== $unsettledCoord) {
                $coord_key = json_encode($front);
                $isRoomCoord = isset($roomCoordToWantChar[$coord_key]);
                if (!isset($coordsInFrontOfRooms[$coord_key])) {
                    if (!$isRoomCoord) {
                        if (!$startedInHallway) {
                           $possible[] = $front;
                        }
                    }
                     elseif ($roomCoordToWantChar[$coord_key] === $unsettledChar)
                     {
                        $isStuckAmphipod = false;
                        $roomHasDeeperOpenSpaces = false;
                        for ($r = $front[0] + 1; $r < count($this->grid) - 1; $r++) {
                            $char = $this->grid[$r][$front[1]];
                            if ($char === ".") {
                                $roomHasDeeperOpenSpaces = true;
                            }
                            if ($char !== "." && $char !== $unsettledChar) {
                                $isStuckAmphipod = true;
                                break;
                            }
                        }
                        if (!$roomHasDeeperOpenSpaces && !$isStuckAmphipod) {
                            $possible[] = $front;
                        }
                     }
                }
            }

            foreach ([[-1, 0], [1, 0], [0, -1], [0, 1]] as $d) {
                $next = [$front[0] + $d[0], $front[1] + $d[1]];
                if (isset($this->grid[$next[0]][$next[1]]) && $this->grid[$next[0]][$next[1]] === ".") {
                    $queue[] = $next;
                }
            }
        }

        return $possible;
    }
}

function parseInput($input) {
    $grid = [];
    foreach (explode("\n", $input) as $line) {
        $grid[] = str_split($line);
    }
    return new State($grid);
}

function calcEnergy($char, $start, $end) {
    $dist = abs($end[1] - $start[1]);
    $dist += $start[0] - 1;
    $dist += $end[0] - 1;

    $energyPerType = [
        "A" => 1,
        "B" => 10,
        "C" => 100,
        "D" => 1000,
    ];

    return $energyPerType[$char] * $dist;
}

$input = file_get_contents("input.txt");
$input = trim($input);

$ans = amphipod($input);
echo $ans;
