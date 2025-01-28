
<?php

/**
 * Class Unit
 * Represents a combat unit (Elf or Goblin).
 */
class Unit {
    public $type;
    public $x;
    public $y;
    public $hp = 200;
    public $ap = 3;

    public function __construct($type, $x, $y) {
        $this->type = $type;
        $this->x = $x;
        $this->y = $y;
    }

    /**
     * Checks if the unit is alive.
     * @return bool True if alive, false otherwise.
     */
    public function isAlive() {
        return $this->hp > 0;
    }
}

/**
 * Class Battle
 * Manages the combat simulation.
 */
class Battle {
    private $map;
    private $units;
    private $rows;
    private $cols;

    public function __construct($map) {
        $this->map = $map;
        $this->rows = count($map);
        $this->cols = strlen($map[0]);
        $this->units = [];
        $this->parseUnits();
    }

    /**
     * Parses the initial positions of units from the map.
     */
    private function parseUnits() {
        for ($y = 0; $y < $this->rows; $y++) {
            for ($x = 0; $x < $this->cols; $x++) {
                if ($this->map[$y][$x] === 'G' || $this->map[$y][$x] === 'E') {
                    $this->units[] = new Unit($this->map[$y][$x], $x, $y);
                }
            }
        }
    }

    /**
     * Simulates the combat until one side is defeated.
     * @return array The outcome of the battle (completed rounds, remaining HP).
     */
    public function simulateCombat() {
        $rounds = 0;
        while (true) {
            usort($this->units, function ($a, $b) {
                if ($a->y !== $b->y) {
                    return $a->y - $b->y;
                }
                return $a->x - $b->x;
            });

            $unitMoved = false;
            foreach ($this->units as $unit) {
              if (!$unit->isAlive()) continue;

              $targets = $this->findTargets($unit);
              if (empty($targets)) {
                  return [$rounds, $this->calculateRemainingHp()];
              }

              if (!$this->isInRange($unit, $targets)) {
                  $this->moveUnit($unit, $targets);
                  $unitMoved = true;
              }
              
              $this->attack($unit, $targets);
            }

            $this->units = array_filter($this->units, function ($unit) {
                return $unit->isAlive();
            });
            
            if ($unitMoved) {
                $rounds++;
            } else {
                
            }
        }
    }

    /**
     * Finds all enemy targets for a given unit.
     * @param Unit $unit The unit.
     * @return array An array of enemy units.
     */
    private function findTargets(Unit $unit) {
        return array_filter($this->units, function ($target) use ($unit) {
            return $target->isAlive() && $target->type !== $unit->type;
        });
    }

    /**
     * Checks if a unit is in range of any target.
     * @param Unit $unit The unit.
     * @param array $targets The enemy targets.
     * @return bool True if in range, false otherwise.
     */
    private function isInRange(Unit $unit, array $targets) {
        foreach ($targets as $target) {
            if (abs($unit->x - $target->x) + abs($unit->y - $target->y) === 1) {
                return true;
            }
        }
        return false;
    }

    /**
     * Moves a unit towards the nearest reachable target.
     * @param Unit $unit The unit to move.
     * @param array $targets The enemy targets.
     */
    private function moveUnit(Unit $unit, array $targets) {
        $inRangeSquares = [];
        foreach ($targets as $target) {
            $inRangeSquares = array_merge($inRangeSquares, $this->getInRangeSquares($target));
        }

        $reachableSquares = [];
        foreach ($inRangeSquares as $square) {
            $path = $this->findShortestPath($unit, $square);
            if ($path !== null) {
                $reachableSquares[] = ['square' => $square, 'path' => $path];
            }
        }

        if (empty($reachableSquares)) {
            return;
        }

        usort($reachableSquares, function ($a, $b) {
            if (count($a['path']) !== count($b['path'])) {
                return count($a['path']) - count($b['path']);
            }
            if ($a['square'][1] !== $b['square'][1]) {
                return $a['square'][1] - $b['square'][1];
            }
            return $a['square'][0] - $b['square'][0];
        });

        $chosenSquare = $reachableSquares[0]['square'];
        $shortestPaths = [];
        foreach ($reachableSquares as $reachable) {
            if ($reachable['square'] === $chosenSquare) {
                $shortestPaths[] = $reachable['path'];
            }
        }

        usort($shortestPaths, function ($a, $b) {
            if ($a[0][1] !== $b[0][1]) {
                return $a[0][1] - $b[0][1];
            }
            return $a[0][0] - $b[0][0];
        });
        
        $nextStep = $shortestPaths[0][0];

        $this->map[$unit->y][$unit->x] = '.';
        $unit->x = $nextStep[0];
        $unit->y = $nextStep[1];
        $this->map[$unit->y][$unit->x] = $unit->type;
    }

    /**
     * Gets the open squares adjacent to a target.
     * @param Unit $target The target unit.
     * @return array An array of adjacent open squares.
     */
    private function getInRangeSquares(Unit $target) {
        $squares = [];
        $potentialSquares = [
            [$target->x, $target->y - 1],
            [$target->x - 1, $target->y],
            [$target->x + 1, $target->y],
            [$target->x, $target->y + 1],
        ];

        foreach ($potentialSquares as $square) {
            if ($square[0] >= 0 && $square[0] < $this->cols && $square[1] >= 0 && $square[1] < $this->rows && $this->map[$square[1]][$square[0]] === '.') {
                $squares[] = $square;
            }
        }
        return $squares;
    }

    /**
     * Finds the shortest path between a unit and a target square using BFS.
     * @param Unit $unit The unit.
     * @param array $targetSquare The target square coordinates.
     * @return array|null The shortest path as an array of coordinates, or null if no path exists.
     */
    private function findShortestPath(Unit $unit, array $targetSquare) {
        $queue = [[[$unit->x, $unit->y], []]];
        $visited = [];
        $directions = [[0, -1], [-1, 0], [1, 0], [0, 1]];

        while (!empty($queue)) {
            list($current, $path) = array_shift($queue);
            if ($current == $targetSquare) {
                return $path;
            }

            $visitedKey = $current[0] . ',' . $current[1];
            if (isset($visited[$visitedKey])) {
                continue;
            }
            $visited[$visitedKey] = true;
            
            foreach ($directions as $dir) {
                $nextX = $current[0] + $dir[0];
                $nextY = $current[1] + $dir[1];

                if ($nextX >= 0 && $nextX < $this->cols && $nextY >= 0 && $nextY < $this->rows && $this->map[$nextY][$nextX] === '.') {
                    $nextPath = $path;
                    $nextPath[] = [$nextX, $nextY];
                    $queue[] = [[$nextX, $nextY], $nextPath];
                }
            }
        }

        return null;
    }

    /**
     * Performs an attack on the adjacent target with the lowest HP.
     * @param Unit $unit The attacking unit.
     * @param array $targets The enemy targets.
     */
    private function attack(Unit $unit, array $targets) {
        $adjacentTargets = [];
        foreach ($targets as $target) {
            if (abs($unit->x - $target->x) + abs($unit->y - $target->y) === 1) {
                $adjacentTargets[] = $target;
            }
        }

        if (empty($adjacentTargets)) {
            return;
        }

        usort($adjacentTargets, function ($a, $b) {
            if ($a->hp !== $b->hp) {
                return $a->hp - $b->hp;
            }
            if ($a->y !== $b->y) {
                return $a->y - $b->y;
            }
            return $a->x - $b->x;
        });

        $target = $adjacentTargets[0];
        $target->hp -= $unit->ap;

        if ($target->hp <= 0) {
            $this->map[$target->y][$target->x] = '.';
        }
    }

    /**
     * Calculates the sum of the hit points of all remaining units.
     * @return int The total remaining hit points.
     */
    private function calculateRemainingHp() {
        $totalHp = 0;
        foreach ($this->units as $unit) {
            if ($unit->isAlive()) {
                $totalHp += $unit->hp;
            }
        }
        return $totalHp;
    }
}

// Read input from file
$input = file('input.txt', FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);

// Create and run the battle simulation
$battle = new Battle($input);
list($rounds, $remainingHp) = $battle->simulateCombat();

// Calculate and print the outcome
$outcome = $rounds * $remainingHp;
echo $outcome . PHP_EOL;
