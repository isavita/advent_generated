
<?php

class Coord {
    public int $x;
    public int $y;

    public function __construct(int $x, int $y) {
        $this->x = $x;
        $this->y = $y;
    }

    public function add(Coord $other): Coord {
        return new Coord($this->x + $other->x, $this->y + $other->y);
    }
}

class Grid {
    public int $width;
    public int $height;
    public array $data;

    public function __construct(int $width, int $height, array $data) {
        $this->width = $width;
        $this->height = $height;
        $this->data = $data;
    }

    public function isInBounds(Coord $coord): bool {
        return 0 <= $coord->x && $coord->x < $this->width && 0 <= $coord->y && $coord->y < $this->height;
    }
}

const EMPTY_CHAR = '.';
const CUBIC_ROCK_CHAR = '#';
const ROUND_ROCK_CHAR = 'O';

const NORTH = new Coord(0, -1);
const WEST = new Coord(-1, 0);
const SOUTH = new Coord(0, 1);
const EAST = new Coord(1, 0);

function buildGrid(array $input): Grid {
    $height = count($input);
    $width = strlen($input[0]);
    $data = [];

    for ($y = 0; $y < $height; $y++) {
        for ($x = 0; $x < $width; $x++) {
            $char = $input[$y][$x];
            if ($char !== EMPTY_CHAR) {
                $data[$x . "," . $y] = $char;
            }
        }
    }

    return new Grid($width, $height, $data);
}

function shiftSingleRock(Grid $grid, Coord $coord, Coord $dir): void {
    $key = $coord->x . "," . $coord->y;
    if (isset($grid->data[$key]) && $grid->data[$key] === ROUND_ROCK_CHAR) {
        $current = $coord;
        $before = $coord->add($dir);
        $beforeKey = $before->x . "," . $before->y;

        while (!isset($grid->data[$beforeKey]) && $grid->isInBounds($before)) {
            $grid->data[$beforeKey] = ROUND_ROCK_CHAR;
            unset($grid->data[$current->x . "," . $current->y]);

            $current = $before;
            $before = $before->add($dir);
            $beforeKey = $before->x . "," . $before->y;
        }
    }
}

function shiftRocks(Grid $grid, Coord $dir): void {
    if ($dir->x === 0 && $dir->y === -1 || $dir->x === -1 && $dir->y === 0) {
        for ($x = 0; $x < $grid->width; $x++) {
            for ($y = 0; $y < $grid->height; $y++) {
                shiftSingleRock($grid, new Coord($x, $y), $dir);
            }
        }
    } else {
        for ($x = $grid->width - 1; $x >= 0; $x--) {
            for ($y = $grid->height - 1; $y >= 0; $y--) {
                shiftSingleRock($grid, new Coord($x, $y), $dir);
            }
        }
    }
}

function cycleRocks(Grid $grid): void {
    shiftRocks($grid, NORTH);
    shiftRocks($grid, WEST);
    shiftRocks($grid, SOUTH);
    shiftRocks($grid, EAST);
}

function calculateGridKey(Grid $grid): int {
    $key = 0;
    foreach ($grid->data as $coordStr => $char) {
        if ($char === ROUND_ROCK_CHAR) {
            list($x, $y) = explode(",", $coordStr);
            $key += (int)$x + (int)$y * $grid->width;
        }
    }
    return $key;
}

function calculateLoad(Grid $grid): int {
    $load = 0;
    foreach ($grid->data as $coordStr => $char) {
        if ($char === ROUND_ROCK_CHAR) {
            list($x, $y) = explode(",", $coordStr);
            $load += $grid->height - (int)$y;
        }
    }
    return $load;
}

function solve(array $input): int {
    $numCycles = 1000000000;
    $grid = buildGrid($input);
    $cache = [];

    for ($i = 0; $i < $numCycles; $i++) {
        $gridKey = calculateGridKey($grid);
        if (isset($cache[$gridKey])) {
            $iStartCycle = $cache[$gridKey];
            $remainingCycles = ($numCycles - $iStartCycle) % ($i - $iStartCycle);
            for ($j = 0; $j < $remainingCycles; $j++) {
                cycleRocks($grid);
            }
            return calculateLoad($grid);
        }
        $cache[$gridKey] = $i;
        cycleRocks($grid);
    }
    return calculateLoad($grid);
}

$input = explode("\n", trim(file_get_contents("input.txt")));
echo solve($input) . "\n";
