
<?php

/**
 * Represents a single unit (Elf or Goblin) in the battle.
 */
class Unit
{
    public int $x;
    public int $y;
    public string $type; // 'E' for Elf, 'G' for Goblin
    public int $hp = 200;
    public int $attackPower = 3;

    public function __construct(int $x, int $y, string $type, int $attackPower = 3)
    {
        $this->x = $x;
        $this->y = $y;
        $this->type = $type;
        $this->attackPower = $attackPower;
    }

    public function isAlive(): bool
    {
        return $this->hp > 0;
    }

    // Reading order comparison
    public function compare(Unit $other): int
    {
        if ($this->y != $other->y) {
            return $this->y <=> $other->y; // Top-to-bottom
        }
        return $this->x <=> $other->x; // Left-to-right
    }
}

/**
 * Represents the battle grid and manages the combat simulation.
 */
class Battle
{
    private array $grid; // 2D array representing the map
    private array $units = []; // Array of Unit objects
    private int $rounds = 0;
    private bool $elfDied = false;  // Part 2: track if any elf has died.
    private int $elfAttackPower;

    public function __construct(array $grid, int $elfAttackPower = 3)
    {
        $this->grid = $grid;
        $this->elfAttackPower = $elfAttackPower;
        $this->initializeUnits();
    }

    /**
     * Reads initial unit positions from the grid.
     */
    private function initializeUnits(): void
    {
        for ($y = 0; $y < count($this->grid); $y++) {
            for ($x = 0; $x < count($this->grid[$y]); $x++) {
                if ($this->grid[$y][$x] == 'E' || $this->grid[$y][$x] == 'G') {
                    $attackPower = ($this->grid[$y][$x] == 'E') ? $this->elfAttackPower : 3;
                    $this->units[] = new Unit($x, $y, $this->grid[$y][$x], $attackPower);
                }
            }
        }
    }


    /**
     * Simulates the combat until one side has no units remaining.
     */
    public function runCombat(): int
    {
        while (true) {
            // Sort units by reading order
            usort($this->units, function (Unit $a, Unit $b) {
                return $a->compare($b);
            });

            $unitTookTurn = false;
            foreach ($this->units as $unit) {
                if (!$unit->isAlive()) {
                    continue;  //Skip the dead units
                }

                // Identify targets
                $targets = $this->findTargets($unit);
                if (empty($targets)) {  //Combat ends
                    if (!$unitTookTurn) {
                        return $this->calculateOutcome();
                    } else {
                        return $this->calculateOutcome(true);
                    }
                }

                // Move
                if (!$this->isInRange($unit, $targets)) {
                    $this->moveUnit($unit, $targets);
                }

                // Attack
                $target = $this->chooseTarget($unit, $targets);
                if ($target) {
                    $this->attack($unit, $target);
                }
                $unitTookTurn = true;
            }
            $this->removeDeadUnits();

            if (count(array_unique(array_column($this->units, 'type'))) <= 1) { // All remaining units of the same type
                return $this->calculateOutcome(true);
            }
            $this->rounds++;
        }
    }

    /**
     * Calculates the final outcome of the battle (rounds * remaining HP).
     */
    private function calculateOutcome($finalRound = false): int
    {
        $totalHp = 0;
        foreach ($this->units as $unit) {
            if ($unit->isAlive()) {
                $totalHp += $unit->hp;
            }
        }
        $completedRounds = $finalRound ? $this->rounds : $this->rounds;
        return $completedRounds * $totalHp;
    }


    /**
     * Finds all enemy units.
     *
     * @param Unit $unit The unit searching for targets.
     * @return Unit[] An array of enemy units.
     */
    private function findTargets(Unit $unit): array
    {
        $targets = [];
        foreach ($this->units as $otherUnit) {
            if ($otherUnit->isAlive() && $otherUnit->type != $unit->type) {
                $targets[] = $otherUnit;
            }
        }
        return $targets;
    }

    /**
     * Checks if a unit is already in range of any target.
     *
     * @param Unit $unit
     * @param Unit[] $targets
     * @return bool
     */
    private function isInRange(Unit $unit, array $targets): bool
    {
        foreach ($targets as $target) {
            if (abs($unit->x - $target->x) + abs($unit->y - $target->y) == 1) {
                return true;
            }
        }
        return false;
    }


    /**
     * Moves the unit towards the nearest reachable target.
     */
    private function moveUnit(Unit $unit, array $targets): void
    {
        $inRangeSquares = [];
        foreach ($targets as $target) {
            $inRangeSquares = array_merge($inRangeSquares, $this->getAdjacentSquares($target->x, $target->y, true));
        }

        $reachableSquares = [];
        foreach ($inRangeSquares as $square) {
            if ($this->isReachable($unit, $square)) {
                $reachableSquares[] = $square;
            }
        }

        if (empty($reachableSquares)) {
            return; // No reachable squares
        }

        $nearestSquares = $this->findNearestSquares($unit, $reachableSquares);
        if (empty($nearestSquares)) {
            return; // No nearest reachable squares
        }

        usort($nearestSquares, function ($a, $b) {  //Sort by reading order
            if ($a[1] != $b[1]) {
                return $a[1] <=> $b[1]; //y
            }
            return $a[0] <=> $b[0]; //x
        });

        $chosenSquare = $nearestSquares[0];

        $bestStep = $this->chooseBestStep($unit, $chosenSquare);

        if ($bestStep) {
            // Update grid and unit position
            $this->grid[$unit->y][$unit->x] = '.';
            $unit->x = $bestStep[0];
            $unit->y = $bestStep[1];
            $this->grid[$unit->y][$unit->x] = $unit->type;
        }
    }

    /**
    * Gets adjacent squares to the given coordinates.
    */
    private function getAdjacentSquares(int $x, int $y, bool $onlyOpen = false): array
    {
        $squares = [];
        $directions = [[0, -1], [-1, 0], [1, 0], [0, 1]]; // Up, Left, Right, Down

        foreach ($directions as $dir) {
            $nx = $x + $dir[0];
            $ny = $y + $dir[1];
            if (isset($this->grid[$ny][$nx])) { // Check if the position is inside the grid
                if (!$onlyOpen || $this->grid[$ny][$nx] == '.') {
                    $squares[] = [$nx, $ny];
                }
            }
        }

        return $squares;
    }

    /**
     * Determines if a square is reachable from the unit's current position.
     */
    private function isReachable(Unit $unit, array $targetSquare): bool
    {
        $visited = [];
        $queue = [[$unit->x, $unit->y, []]]; // x, y, path

        while (!empty($queue)) {
            [$x, $y, $path] = array_shift($queue);

            if ($x == $targetSquare[0] && $y == $targetSquare[1]) {
                return true;
            }

            $key = $x . ',' . $y;
            if (isset($visited[$key])) {
                continue;
            }
            $visited[$key] = true;

            $adjacent = $this->getAdjacentSquares($x, $y);
            foreach ($adjacent as $adj) {
                if ($this->grid[$adj[1]][$adj[0]] == '.' || ($adj[0] == $unit->x && $adj[1] == $unit->y)) {
                    $newPath = $path;
                    $newPath[] = $adj;
                    $queue[] = [$adj[0], $adj[1], $newPath];
                }
            }
        }
        return false;
    }

    /**
     * Finds the nearest squares from a list of reachable squares.
     */
    private function findNearestSquares(Unit $unit, array $reachableSquares): array
    {
        $minDistance = PHP_INT_MAX;
        $nearestSquares = [];
        $distances = $this->calculateDistances($unit);

        foreach ($reachableSquares as $square) {
            $distance = $distances[$square[1]][$square[0]];
            if ($distance < $minDistance) {
                $minDistance = $distance;
                $nearestSquares = [$square];
            } elseif ($distance == $minDistance) {
                $nearestSquares[] = $square;
            }
        }
        return $nearestSquares;
    }

    /**
     * Chooses the best step to take towards the chosen square, considering reading order.
     */
    private function chooseBestStep(Unit $unit, array $chosenSquare): ?array
    {
        $shortestPaths = $this->findShortestPaths($unit, $chosenSquare);
        if (empty($shortestPaths)) {
            return null;
        }

        // Sort paths based on the reading order of the *first step*
        usort($shortestPaths, function ($a, $b) {
            // Compare first steps: [x, y]
            if ($a[0][1] != $b[0][1]) { //y
                return $a[0][1] <=> $b[0][1];
            }
            return $a[0][0] <=> $b[0][0]; //x
        });

        // Return the first step of the best path
        return $shortestPaths[0][0];
    }


    /**
    * Finds all the shortest paths.
    */
    private function findShortestPaths(Unit $unit, array $targetSquare): array
    {
        $visited = [];
        $queue = [[$unit->x, $unit->y, []]]; // x, y, path (path stores the STEPS)
        $shortestPaths = [];
        $minPathLength = PHP_INT_MAX;

        while (!empty($queue)) {
            [$x, $y, $path] = array_shift($queue);

            if ($x == $targetSquare[0] && $y == $targetSquare[1]) {
                if (count($path) < $minPathLength) {
                    $minPathLength = count($path);
                    $shortestPaths = [$path];
                } elseif (count($path) == $minPathLength) {
                    $shortestPaths[] = $path;
                }
                continue;
            }

            $key = $x . ',' . $y;
            if (isset($visited[$key])) {
                continue;
            }
            $visited[$key] = true;

            $adjacent = $this->getAdjacentSquares($x, $y);
            foreach ($adjacent as $adj) {
                // Allow to revisit the unit starting cell
                if ($this->grid[$adj[1]][$adj[0]] == '.' || ($adj[0] == $unit->x && $adj[1] == $unit->y)) {
                    $newPath = $path;
                    $newPath[] = $adj;  // Add the step to the path
                    $queue[] = [$adj[0], $adj[1], $newPath];
                }
            }
        }

        return $shortestPaths;
    }

    /**
    * Calculates distances to the unit.
    */
    private function calculateDistances(Unit $unit): array
    {
        $distances = array_fill(0, count($this->grid), array_fill(0, count($this->grid[0]), PHP_INT_MAX));
        $distances[$unit->y][$unit->x] = 0;
        $queue = [[$unit->x, $unit->y]];
        $visited = [];

        while (!empty($queue)) {
            [$x, $y] = array_shift($queue);
            $key = "$x,$y";

            if (isset($visited[$key])) {
                continue;
            }
            $visited[$key] = true;

            $adjacent = $this->getAdjacentSquares($x, $y);
            foreach ($adjacent as $adj) {
                if ($this->grid[$adj[1]][$adj[0]] == '.' || ($adj[0] == $unit->x && $adj[1] == $unit->y)) {
                    $distances[$adj[1]][$adj[0]] = min($distances[$adj[1]][$adj[0]], $distances[$y][$x] + 1);
                    $queue[] = [$adj[0], $adj[1]];
                }
            }
        }
        return $distances;
    }


    /**
     * Chooses a target to attack based on lowest HP and reading order.
     *
     * @param Unit $unit
     * @param array $targets
     * @return Unit|null
     */
    private function chooseTarget(Unit $unit, array $targets): ?Unit
    {
        $inRangeTargets = [];
        foreach ($targets as $target) {
            if (abs($unit->x - $target->x) + abs($unit->y - $target->y) == 1) {
                $inRangeTargets[] = $target;
            }
        }

        if (empty($inRangeTargets)) {
            return null;
        }

        usort($inRangeTargets, function (Unit $a, Unit $b) {
            if ($a->hp != $b->hp) {
                return $a->hp <=> $b->hp; // Lowest HP
            }
            return $a->compare($b); // Reading order
        });

        return $inRangeTargets[0];
    }

    /**
     * Performs the attack action.
     *
     * @param Unit $attacker
     * @param Unit $target
     */
    private function attack(Unit $attacker, Unit $target): void
    {
        $target->hp -= $attacker->attackPower;
        if ($target->hp <= 0) {
            if ($target->type == 'E') {  //Check if elf is dead
                $this->elfDied = true;
            }
            $this->grid[$target->y][$target->x] = '.'; // Remove from grid
        }
    }

    /**
     * Removes dead units from the units array.
     */
    private function removeDeadUnits(): void
    {
        $this->units = array_filter($this->units, function (Unit $unit) {
            return $unit->isAlive();
        });
    }

    /**
    * Checks if an elf died.
    */
    public function elfDied(): bool
    {
        return $this->elfDied;
    }
}


// --- Main Execution ---

$input = file('input.txt', FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
$grid = [];
foreach ($input as $line) {
    $grid[] = str_split(trim($line));
}

// Part 1
$battle1 = new Battle($grid);
$outcome1 = $battle1->runCombat();
echo "Part 1: " . $outcome1 . "\n";


// Part 2
$elfAttackPower = 4;
while (true) {
    $battle2 = new Battle($grid, $elfAttackPower);
    $outcome2 = $battle2->runCombat();
    if (!$battle2->elfDied()) {
        echo "Part 2: " . $outcome2 . " (Elf Attack Power: " . $elfAttackPower . ")\n";
        break;
    }
    $elfAttackPower++;
}

?>
