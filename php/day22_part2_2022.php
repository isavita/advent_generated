
<?php

class P {
    public $x;
    public $y;

    public function __construct(int $x, int $y) {
        $this->x = $x;
        $this->y = $y;
    }

    public function __toString(): string {
        return "({$this->x}, {$this->y})";
    }
}

class Dir {
    const N = 0;
    const E = 1;
    const S = 2;
    const W = 3;

    public static function rotate(int $direction, string $rotate): int {
        if ($rotate == 'R') {
            return ($direction + 1) % 4;
        } elseif ($rotate == 'L') {
            return ($direction - 1 + 4) % 4;
        }
        return $direction;
    }

    public static function points(int $dir): int {
        return ($dir + 3) % 4;
    }
}

class Movement {
    public $steps;
    public $rotate;

    public function __construct(int $steps = 0, string $rotate = null) {
        $this->steps = $steps;
        $this->rotate = $rotate;
    }
}

class Human {
    public $curr;
    public $facing;

    public function __construct(P $curr, int $facing) {
        $this->curr = $curr;
        $this->facing = $facing;
    }

    public function walk(array $map_data, array $dirs, int $size): array {
        $dir_delta = $dirs[$this->facing];
        $next_pos = new P($this->curr->x + $dir_delta->x, $this->curr->y + $dir_delta->y);

        if (isset($map_data[$this->pos_to_key($next_pos)])) {
            if ($map_data[$this->pos_to_key($next_pos)]) {
                return [$this->curr, $this->facing];
            } else {
                return [$next_pos, $this->facing];
            }
        } else {
            list($new_pos, $new_facing) = $this->cross_border($next_pos, $this->facing, $size);
            if (isset($map_data[$this->pos_to_key($new_pos)]) && $map_data[$this->pos_to_key($new_pos)]) {
                return [$this->curr, $this->facing];
            }
            return [$new_pos, $new_facing];
        }
    }

    private function pos_to_key(P $pos): string {
        return $pos->x . "," . $pos->y;
    }


    private function cross_border(P $n, int $dir, int $size): array {
        $x = $n->x;
        $y = $n->y;
        $S = $size;

        if ($x == -1 && $y < 2 * $S) {
            return [new P($y + 2 * $S, $x + 1), Dir::E];
        } elseif ($x == -1 && $y >= 2 * $S) {
            return [new P($x + 4 * $S, $y - 2 * $S), Dir::N];
        } elseif ($x == $S && $dir == Dir::S) {
            return [new P($y - $S, $x + $S - 1), Dir::W];
        } elseif ($x == 2 * $S - 1 && $dir == Dir::N) {
            return [new P($y + $S, $x - $S + 1), Dir::E];
        } elseif ($x == 3 * $S && $dir == Dir::S) {
            return [new P($y + 2 * $S, $x - 2 * $S - 1), Dir::W];
        } elseif ($x == 4 * $S) {
            return [new P($x - 4 * $S, $y + 2 * $S), Dir::S];
        } elseif ($y == -1 && $x < 3 * $S) {
            return [new P(3 * $S - 1 - $x, $y + $S + 1), Dir::E];
        } elseif ($y == -1 && $x >= 3 * $S) {
            return [new P($y + 1, $x - 2 * $S), Dir::S];
        } elseif ($y == $S - 1 && $x < $S) {
            return [new P(3 * $S - 1 - $x, $y - $S + 1), Dir::E];
        } elseif ($y == $S - 1 && $x >= $S && $dir == Dir::W) {
            return [new P($y + $S + 1, $x - $S), Dir::S];
        } elseif ($y == $S && $dir == Dir::E) {
            return [new P($y + 2 * $S - 1, $x - 2 * $S), Dir::N];
        } elseif ($y == 2 * $S && $x < 2 * $S && $dir == Dir::E) {
            return [new P($y - $S - 1, $x + $S), Dir::N];
        } elseif ($y == 2 * $S && $x >= 2 * $S) {
            return [new P(3 * $S - 1 - $x, $y + $S - 1), Dir::W];
        } elseif ($y == 3 * $S) {
            return [new P(3 * $S - 1 - $x, $y - $S - 1), Dir::W];
        } else {
            throw new Exception("Not a border crossing");
        }
    }
}

function parse_path(string $path): array {
    $movements = [];
    $acc = 0;
    for ($i = 0; $i < strlen($path); $i++) {
        $char = $path[$i];
        if (in_array($char, ['R', 'L'])) {
            if ($acc != 0) {
                $movements[] = new Movement($acc);
                $acc = 0;
            }
            $movements[] = new Movement(0, $char);
        } elseif (is_numeric($char)) {
            $acc = $acc * 10 + (int)$char;
        }
    }
    if ($acc != 0) {
        $movements[] = new Movement($acc);
    }
    return $movements;
}

function parse_input(string $filename): array {
    $map_data = [];
    $size = 0;
    $movements = [];

    $lines = file($filename, FILE_IGNORE_NEW_LINES);

    $r = 0;
    foreach ($lines as $line) {
        if ($line == "") {
            break;
        }
        if ($r == 0) {
            $size = strlen($line) / 3;
        }
        for ($c = 0; $c < strlen($line); $c++) {
            $char = $line[$c];
            if ($char == ' ') {
                continue;
            } elseif ($char == '#') {
                $map_data[(new P($r, $c))->x . "," . (new P($r, $c))->y] = true;
            } elseif ($char == '.') {
                $map_data[(new P($r, $c))->x . "," . (new P($r, $c))->y] = false;
            }
        }
        $r++;
    }

    $movement_line = $lines[$r + 1];
    $movements = parse_path($movement_line);

    return [$map_data, $size, $movements];
}

function main() {
    list($map_data, $size, $movements) = parse_input("input.txt");
    $dirs = [
        Dir::N => new P(-1, 0),
        Dir::E => new P(0, 1),
        Dir::S => new P(1, 0),
        Dir::W => new P(0, -1)
    ];

    $human = new Human(new P(0, $size), Dir::E);

    foreach ($movements as $mov) {
        if ($mov->rotate) {
            $human->facing = Dir::rotate($human->facing, $mov->rotate);
        }
        for ($i = 0; $i < $mov->steps; $i++) {
            list($new_pos, $new_facing) = $human->walk($map_data, $dirs, $size);
            if ($new_pos === $human->curr && $new_facing === $human->facing) {
                break;
            }
            $human->curr = $new_pos;
            $human->facing = $new_facing;
        }
    }

    $final_value = 1000 * ($human->curr->x + 1) + 4 * ($human->curr->y + 1) + Dir::points($human->facing);
    echo $final_value . PHP_EOL;
}

main();
