
<?php

$input = file_get_contents("input.txt");
$input = trim($input);

list($nums, $boards) = parseInput($input);

$lastWinningScore = -1;
$alreadyWon = [];
foreach ($nums as $n) {
    foreach ($boards as $bi => $b) {
        if (isset($alreadyWon[$bi])) {
            continue;
        }
        $didWin = $b->pickNum($n);
        if ($didWin) {
            $lastWinningScore = $b->score() * $n;
            $alreadyWon[$bi] = true;
        }
    }
}

echo $lastWinningScore . PHP_EOL;

class BoardState {
    private $board;
    private $picked;

    public function __construct($board) {
        $this->board = $board;
        $this->picked = array_fill(0, count($board), array_fill(0, count($board[0]), false));
    }

    public function pickNum($num) {
        foreach ($this->board as $r => $rows) {
            foreach ($rows as $c => $v) {
                if ($v == $num) {
                    $this->picked[$r][$c] = true;
                }
            }
        }

        for ($i = 0; $i < count($this->board); $i++) {
            $isFullRow = true;
            $isFullCol = true;

            for ($j = 0; $j < count($this->board); $j++) {
                if (!$this->picked[$i][$j]) {
                    $isFullRow = false;
                }

                if (!$this->picked[$j][$i]) {
                    $isFullCol = false;
                }
            }
            if ($isFullRow || $isFullCol) {
                return true;
            }
        }

        return false;
    }

    public function score() {
        $score = 0;

        foreach ($this->board as $r => $rows) {
            foreach ($rows as $c => $v) {
                if (!$this->picked[$r][$c]) {
                    $score += $v;
                }
            }
        }

        return $score;
    }
}

function parseInput($input) {
    $lines = explode("\n\n", $input);
    $nums = array_map('toInt', explode(",", $lines[0]));

    $boards = [];
    foreach (array_slice($lines, 1) as $grid) {
        $b = [];
        foreach (explode("\n", $grid) as $line) {
            $line = str_replace("  ", " ", $line);
            while ($line[0] == ' ') {
                $line = substr($line, 1);
            }
            $parts = explode(" ", $line);

            $row = [];
            foreach ($parts as $p) {
                $row[] = toInt($p);
            }
            $b[] = $row;
        }

        $boards[] = new BoardState($b);
    }

    return [$nums, $boards];
}

function toInt($s) {
    return intval($s);
}
?>
