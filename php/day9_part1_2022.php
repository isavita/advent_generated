<?php
$file = fopen("input.txt", "r");

class Point {
    public $x, $y;

    public function __construct($x, $y) {
        $this->x = $x;
        $this->y = $y;
    }

    public function __toString(): string
    {
        return "({$this->x},{$this->y})";
    }
}

$head = new Point(0, 0);
$tail = new Point(0, 0);
$visited = [strval($tail) => true];

while (($line = fgets($file)) !== false) {
    $parts = explode(" ", trim($line));
    $dir = $parts[0];
    $steps = intval($parts[1]);

    for ($i = 0; $i < $steps; $i++) {
        switch ($dir) {
            case "R":
                $head->x++;
                break;
            case "L":
                $head->x--;
                break;
            case "U":
                $head->y++;
                break;
            case "D":
                $head->y--;
                break;
        }

        if (abs($head->x - $tail->x) > 1 || abs($head->y - $tail->y) > 1) {
            if ($head->x != $tail->x && $head->y != $tail->y) {
                if ($head->x > $tail->x) {
                    $tail->x++;
                } else {
                    $tail->x--;
                }
                if ($head->y > $tail->y) {
                    $tail->y++;
                } else {
                    $tail->y--;
                }
            } else {
                if ($head->x > $tail->x) {
                    $tail->x++;
                } elseif ($head->x < $tail->x) {
                    $tail->x--;
                }
                if ($head->y > $tail->y) {
                    $tail->y++;
                } elseif ($head->y < $tail->y) {
                    $tail->y--;
                }
            }
        }

        $visited[strval($tail)] = true;
    }
}

fclose($file);
echo count($visited) . "\n";