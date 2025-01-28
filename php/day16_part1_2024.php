
<?php

class ReindeerMaze
{
    private $maze;
    private $startRow;
    private $startCol;
    private $endRow;
    private $endCol;
    private $rows;
    private $cols;

    public function __construct(string $filename)
    {
        $this->loadMaze($filename);
    }

    private function loadMaze(string $filename): void
    {
        $this->maze = [];
        $file = fopen($filename, 'r');
        $row = 0;
        while (($line = fgets($file)) !== false) {
            $line = rtrim($line, "\r\n");
            $this->maze[] = str_split($line);
            $col = 0;
            foreach ($this->maze[$row] as $char) {
                if ($char === 'S') {
                    $this->startRow = $row;
                    $this->startCol = $col;
                } elseif ($char === 'E') {
                    $this->endRow = $row;
                    $this->endCol = $col;
                }
                $col++;
            }
            $row++;
        }
        fclose($file);
        $this->rows = count($this->maze);
        $this->cols = count($this->maze[0]);
    }

    public function findLowestScore(): int
    {
        $dist = [];
        for ($r = 0; $r < $this->rows; $r++) {
            for ($c = 0; $c < $this->cols; $c++) {
                for ($d = 0; $d < 4; $d++) {
                    $dist[$r][$c][$d] = INF;
                }
            }
        }
        $dist[$this->startRow][$this->startCol][0] = 0; // Start facing East (0)

        $pq = new SplPriorityQueue();
        $pq->insert([0, $this->startRow, $this->startCol, 0], -0); // Priority queue sorts by lowest, so use negative score. [score, row, col, dir]

        $dr = [0, 1, 0, -1]; // E, S, W, N
        $dc = [1, 0, -1, 0];

        while (!$pq->isEmpty()) {
            $current = $pq->extract();
            $score = $current[0];
            $r = $current[1];
            $c = $current[2];
            $dir = $current[3];

            if ($r === $this->endRow && $c === $this->endCol) {
                return $score;
            }

            if ($score > $dist[$r][$c][$dir]) {
                continue;
            }

            // Move forward
            $nr = $r + $dr[$dir];
            $nc = $c + $dc[$dir];
            if ($nr >= 0 && $nr < $this->rows && $nc >= 0 && $nc < $this->cols && $this->maze[$nr][$nc] !== '#') {
                $newScore = $score + 1;
                if ($newScore < $dist[$nr][$nc][$dir]) {
                    $dist[$nr][$nc][$dir] = $newScore;
                    $pq->insert([$newScore, $nr, $nc, $dir], -$newScore);
                }
            }

            // Rotate clockwise
            $newDirCw = ($dir + 1) % 4;
            $rotateCost = 1000;
            $newScoreCw = $score + $rotateCost;
            if ($newScoreCw < $dist[$r][$c][$newDirCw]) {
                $dist[$r][$c][$newDirCw] = $newScoreCw;
                $pq->insert([$newScoreCw, $r, $c, $newDirCw], -$newScoreCw);
            }

            // Rotate counter-clockwise
            $newDirCcw = ($dir - 1 + 4) % 4;
            $newScoreCcw = $score + $rotateCost;
            if ($newScoreCcw < $dist[$r][$c][$newDirCcw]) {
                $dist[$r][$c][$newDirCcw] = $newScoreCcw;
                $pq->insert([$newScoreCcw, $r, $c, $newDirCcw], -$newScoreCcw);
            }
        }

        return -1; // No path found (should not happen based on problem description)
    }
}

$mazeSolver = new ReindeerMaze('input.txt');
$lowestScore = $mazeSolver->findLowestScore();
echo $lowestScore . "\n";

?>
