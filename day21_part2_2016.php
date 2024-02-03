
<?php

class Scrambler {
    private $pw;

    public function __construct($pw) {
        $this->pw = str_split($pw);
    }

    public function swapPositions($x, $y) {
        list($this->pw[$x], $this->pw[$y]) = array($this->pw[$y], $this->pw[$x]);
    }

    public function swapLetters($x, $y) {
        $this->swapPositions(array_search($x, $this->pw), array_search($y, $this->pw));
    }

    public function rotate($steps) {
        $length = count($this->pw);
        $steps = $steps % $length;
        if ($steps < 0) {
            $steps += $length;
        }
        $this->pw = array_merge(array_slice($this->pw, $length - $steps), array_slice($this->pw, 0, $length - $steps));
    }

    public function rotateLetter($x) {
        $index = array_search($x, $this->pw);
        if ($index >= 4) {
            $index++;
        }
        $this->rotate($index + 1);
    }

    public function derotateLetter($x) {
        $index = array_search($x, $this->pw);
        if ($index % 2 == 1) {
            $rot = -($index + 1) / 2;
        } elseif ($index != 0) {
            $rot = (6 - $index) / 2;
        } else {
            $rot = -1;
        }
        $this->rotate($rot);
    }

    public function reverse($x, $y) {
        $this->pw = array_merge(array_slice($this->pw, 0, $x), array_reverse(array_slice($this->pw, $x, $y - $x + 1)), array_slice($this->pw, $y + 1));
    }

    public function move($x, $y) {
        $ch = $this->pw[$x];
        if ($x < $y) {
            array_splice($this->pw, $x, 1);
            array_splice($this->pw, $y, 0, $ch);
        } else {
            array_splice($this->pw, $y, 0, $ch);
            array_splice($this->pw, $x + 1, 1);
        }
    }

    public function scramble($instructions, $direction) {
        if ($direction < 0) {
            $instructions = array_reverse($instructions);
        }
        foreach ($instructions as $instruction) {
            $line = explode(" ", $instruction);
            switch (true) {
                case strpos($instruction, "swap") === 0:
                    $x = $line[2];
                    $y = $line[count($line) - 1];
                    if ($line[1] == "position") {
                        $this->swapPositions((int)$x, (int)$y);
                    } else {
                        $this->swapLetters($x[0], $y[0]);
                    }
                    break;
                case strpos($instruction, "rotate") === 0:
                    if ($line[1] == "based") {
                        if ($direction > 0) {
                            $this->rotateLetter($line[count($line) - 1][0]);
                        } else {
                            $this->derotateLetter($line[count($line) - 1][0]);
                        }
                    } else {
                        $x = (int)$line[2];
                        if ($line[1] == "left") {
                            $x = -$x;
                        }
                        if ($direction < 0) {
                            $x = -$x;
                        }
                        $this->rotate($x);
                    }
                    break;
                case strpos($instruction, "reverse") === 0:
                    $x = $line[2];
                    $y = $line[count($line) - 1];
                    $this->reverse((int)$x, (int)$y);
                    break;
                case strpos($instruction, "move") === 0:
                    $x = $line[2];
                    $y = $line[count($line) - 1];
                    $xi = (int)$x;
                    $yi = (int)$y;
                    if ($direction < 0) {
                        list($xi, $yi) = array($yi, $xi);
                    }
                    $this->move($xi, $yi);
                    break;
            }
        }
        return $this;
    }

    public function unscramble($instructions) {
        return $this->scramble($instructions, -1);
    }

    public function __toString() {
        return implode("", $this->pw);
    }
}

$instructions = file("input.txt", FILE_IGNORE_NEW_LINES);
$hashed = "fbgdceah";
$scrambler = new Scrambler($hashed);
$result = $scrambler->unscramble($instructions);
echo $result;
?>
