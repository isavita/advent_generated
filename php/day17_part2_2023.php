
<?php

/**
 * Class ClumsyCrucible
 * Solves the Clumsy Crucible problem (Day 17 of Advent of Code).
 */
class ClumsyCrucible
{
    private array $grid;
    private int $rows;
    private int $cols;

    /**
     * ClumsyCrucible constructor.
     * @param string $inputFile Path to the input file.
     */
    public function __construct(string $inputFile)
    {
        $this->grid = $this->readInput($inputFile);
        $this->rows = count($this->grid);
        $this->cols = count($this->grid[0]);
    }

    /**
     * Reads the input grid from the file.
     * @param string $inputFile Path to the input file.
     * @return array The grid as a 2D array of integers.
     */
    private function readInput(string $inputFile): array
    {
        $grid = [];
        $lines = file($inputFile, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
        foreach ($lines as $line) {
            $grid[] = array_map('intval', str_split($line));
        }
        return $grid;
    }

    /**
     * Solves Part One of the problem.
     * @return int The minimum heat loss.
     */
    public function solvePartOne(): int
    {
        return $this->findMinHeatLoss(1, 3);
    }

    /**
     * Solves Part Two of the problem.
     * @return int The minimum heat loss.
     */
    public function solvePartTwo(): int
    {
        return $this->findMinHeatLoss(4, 10);
    }

    /**
     * Finds the minimum heat loss using Dijkstra's algorithm with modifications for movement constraints.
     * @param int $minSteps Minimum steps in one direction before turning.
     * @param int $maxSteps Maximum steps in one direction before turning.
     * @return int The minimum heat loss.
     */
    private function findMinHeatLoss(int $minSteps, int $maxSteps): int
    {
        $start = [0, 0, 0, 0, 0]; // row, col, direction (0: right, 1: down, 2: left, 3: up), steps, heatLoss
        $queue = new SplPriorityQueue();
        $queue->setExtractFlags(SplPriorityQueue::EXTR_DATA);
        $queue->insert($start, 0);

        $visited = [];
        $directions = [[0, 1], [1, 0], [0, -1], [-1, 0]];

        while (!$queue->isEmpty()) {
            [$row, $col, $dir, $steps, $heatLoss] = $queue->extract();

            if ($row === $this->rows - 1 && $col === $this->cols - 1 && $steps >= $minSteps) {
                return $heatLoss;
            }

            $state = "$row,$col,$dir,$steps";
            if (isset($visited[$state])) {
                continue;
            }
            $visited[$state] = true;

            // Try moving in the same direction
            if ($steps < $maxSteps) {
                $newRow = $row + $directions[$dir][0];
                $newCol = $col + $directions[$dir][1];
                if ($this->isValid($newRow, $newCol)) {
                    $newHeatLoss = $heatLoss + $this->grid[$newRow][$newCol];
                    $queue->insert([$newRow, $newCol, $dir, $steps + 1, $newHeatLoss], -$newHeatLoss);
                }
            }

            // Try turning left and right
            if ($steps >= $minSteps || ($row === 0 && $col === 0)) {
              for ($turn = -1; $turn <= 1; $turn += 2) {
                  $newDir = ($dir + $turn + 4) % 4;
                  $newRow = $row + $directions[$newDir][0];
                  $newCol = $col + $directions[$newDir][1];
                  if ($this->isValid($newRow, $newCol)) {
                      $newHeatLoss = $heatLoss + $this->grid[$newRow][$newCol];
                      $queue->insert([$newRow, $newCol, $newDir, 1, $newHeatLoss], -$newHeatLoss);
                  }
              }
            }
        }

        return -1; // Should not happen
    }

    /**
     * Checks if a given position is within the grid bounds.
     * @param int $row The row index.
     * @param int $col The column index.
     * @return bool True if the position is valid, false otherwise.
     */
    private function isValid(int $row, int $col): bool
    {
        return $row >= 0 && $row < $this->rows && $col >= 0 && $col < $this->cols;
    }
}

// Main execution
$solver = new ClumsyCrucible('input.txt');
$partOneResult = $solver->solvePartOne();
$partTwoResult = $solver->solvePartTwo();

echo "Part One: " . $partOneResult . "\n";
echo "Part Two: " . $partTwoResult . "\n";

?>
