<?php

class BingoBoard {
    public $numbers = [];
    public $marked = [];

    public function __construct() {
        $this->numbers = array_fill(0, 5, array_fill(0, 5, 0));
        $this->marked = array_fill(0, 5, array_fill(0, 5, false));
    }

    public function mark($number) {
        foreach ($this->numbers as $i => $row) {
            foreach ($row as $j => $n) {
                if ($n == $number) {
                    $this->marked[$i][$j] = true;
                }
            }
        }
    }

    public function hasWon() {
        for ($i = 0; $i < 5; $i++) {
            if ($this->isRowMarked($i) || $this->isColumnMarked($i)) {
                return true;
            }
        }
        return false;
    }

    public function unmarkedSum() {
        $sum = 0;
        foreach ($this->numbers as $i => $row) {
            foreach ($row as $j => $n) {
                if (!$this->marked[$i][$j]) {
                    $sum += $n;
                }
            }
        }
        return $sum;
    }

    private function isRowMarked($rowIndex) {
        foreach ($this->marked[$rowIndex] as $marked) {
            if (!$marked) {
                return false;
            }
        }
        return true;
    }

    private function isColumnMarked($columnIndex) {
        foreach ($this->marked as $row) {
            if (!$row[$columnIndex]) {
                return false;
            }
        }
        return true;
    }
}

$file = fopen("input.txt", "r");
$firstLine = true;
$numbers = [];
$boards = [];
$currentBoard = null;
$rowIndex = 0;

while (($line = fgets($file)) !== false) {
    if ($firstLine) {
        $numbers = explode(",", trim($line));
        $firstLine = false;
        continue;
    }
    if (trim($line) == "") {
        if ($currentBoard !== null) {
            $boards[] = $currentBoard;
        }
        $currentBoard = new BingoBoard();
        $rowIndex = 0;
    } else {
        $currentBoard->numbers[$rowIndex] = array_map('intval', preg_split('/\s+/', trim($line)));
        $rowIndex++;
    }
}
if ($currentBoard !== null) {
    $boards[] = $currentBoard;
}

fclose($file);

$winningBoard = null;
$winningNumber = 0;

foreach ($numbers as $number) {
    $number = intval($number);
    foreach ($boards as $board) {
        $board->mark($number);
        if ($board->hasWon()) {
            $winningBoard = $board;
            $winningNumber = $number;
            break 2;
        }
    }
}

if ($winningBoard) {
    echo $winningBoard->unmarkedSum() * $winningNumber;
}

?>