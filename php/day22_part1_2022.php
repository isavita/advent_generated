
<?php

class P {
    public int $x;
    public int $y;

    public function __construct(int $x, int $y) {
        $this->x = $x;
        $this->y = $y;
    }
}

enum Dir: int {
    case N = 0;
    case E = 1;
    case S = 2;
    case W = 3;

    public function rotate(string $direction): Dir {
        return match ($direction) {
            'R' => Dir::from(($this->value + 1) % 4),
            'L' => Dir::from(($this->value - 1 + 4) % 4),
            default => $this,
        };
    }

    public function points(): int {
        return ($this->value + 3) % 4;
    }
}

class Movement {
    public int $steps;
    public string $rotate;

    public function __construct(int $steps = 0, string $rotate = '') {
        $this->steps = $steps;
        $this->rotate = $rotate;
    }
}

class Human {
    public P $curr;
    public Dir $facing;

    public function __construct(P $curr, Dir $facing) {
        $this->curr = $curr;
        $this->facing = $facing;
    }
}

$map = [];
$size = 0;
$movements = [];
$dirs = [
    new P(-1, 0), // N
    new P(0, 1),  // E
    new P(1, 0),  // S
    new P(0, -1), // W
];

function parse(): void {
    global $map, $size, $movements;

    $file = fopen("input.txt", "r");
    if (!$file) {
        throw new Exception("Failed to open input.txt");
    }

    $r = 0;
    while (($line = fgets($file)) !== false) {
        $line = rtrim($line, "\r\n");
        if ($line === "") {
            break;
        }

        if ($r === 0) {
            $size = strlen($line) / 3;
        }

        for ($c = 0; $c < strlen($line); $c++) {
            $char = $line[$c];
            if ($char === ' ') {
                continue;
            }
            $map[serialize(new P($r, $c))] = $char === '#';
        }
        $r++;
    }
    
    $path = fgets($file);
    if ($path === false) {
        throw new Exception("Failed to read path from input.txt");
    }
    $movements = parsePath(rtrim($path, "\r\n"));
    fclose($file);
}

function parsePath(string $path): array {
    $movements = [];
    $acc = 0;
    for ($i = 0; $i < strlen($path); $i++) {
        $char = $path[$i];
        switch ($char) {
            case 'R':
                $movements[] = new Movement($acc);
                $acc = 0;
                $movements[] = new Movement(rotate: 'R');
                break;
            case 'L':
                $movements[] = new Movement($acc);
                $acc = 0;
                $movements[] = new Movement(rotate: 'L');
                break;
            default:
                $acc = 10 * $acc + (int)$char;
        }
    }
    $movements[] = new Movement($acc);
    return $movements;
}

function walk(Human $human): bool {
    global $map, $dirs;
    $dir = $dirs[$human->facing->value];
    $next = new P($human->curr->x + $dir->x, $human->curr->y + $dir->y);
    $key = serialize($next);
    if (isset($map[$key])) {
        if ($map[$key]) {
            return false;
        }
        $human->curr = $next;
        return true;
    }

    $oppDir = new P(-$dir->x, -$dir->y);
    while (true) {
        $lookAhead = new P($next->x + $oppDir->x, $next->y + $oppDir->y);
        $lookAheadKey = serialize($lookAhead);
        if (!isset($map[$lookAheadKey])) {
            if ($map[serialize($next)]) {
                return false;
            }
            $human->curr = $next;
            return true;
        }
        $next = $lookAhead;
    }
}

parse();

$human = new Human(new P(0, $size), Dir::E);

foreach ($movements as $mov) {
    $human->facing = $human->facing->rotate($mov->rotate);
    for ($i = 0; $i < $mov->steps; $i++) {
        if (!walk($human)) {
            break;
        }
    }
}

echo 1000 * ($human->curr->x + 1) + 4 * ($human->curr->y + 1) + $human->facing->points() . PHP_EOL;
