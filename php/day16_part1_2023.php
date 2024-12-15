
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

    public function rotate90(): Coord {
        return new Coord($this->y, -$this->x);
    }

    public function rotateNeg90(): Coord {
        return new Coord(-$this->y, $this->x);
    }

    public function isInBounds(Grid $grid): bool {
        return 0 <= $this->x && $this->x < $grid->width && 0 <= $this->y && $this->y < $grid->height;
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
}

class Beam {
    public Coord $origin;
    public Coord $dir;

    public function __construct(Coord $origin, Coord $dir) {
        $this->origin = $origin;
        $this->dir = $dir;
    }
}

const EMPTY_CHAR = '.';
const ASCENDING_MIRROR = '/';
const DESCENDING_MIRROR = '\\';
const VERTICAL_SPLITTER = '|';
const HORIZONTAL_SPLITTER = '-';

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
                $data[serialize(new Coord($x, $y))] = $char;
            }
        }
    }

    return new Grid($width, $height, $data);
}

function nextBeam(Grid $grid, Beam $beam): array {
    $beams = [];
    $key = serialize($beam->origin);
    $char = $grid->data[$key] ?? null;

    if ($char === null) {
        $newBeam = new Beam($beam->origin->add($beam->dir), $beam->dir);
        $beams[] = $newBeam;
        return $beams;
    }

    switch ($char) {
        case ASCENDING_MIRROR:
            $newDir = ($beam->dir == NORTH || $beam->dir == SOUTH) ? $beam->dir->rotateNeg90() : $beam->dir->rotate90();
            $newBeam = new Beam($beam->origin->add($newDir), $newDir);
            $beams[] = $newBeam;
            break;
        case DESCENDING_MIRROR:
            $newDir = ($beam->dir == NORTH || $beam->dir == SOUTH) ? $beam->dir->rotate90() : $beam->dir->rotateNeg90();
            $newBeam = new Beam($beam->origin->add($newDir), $newDir);
            $beams[] = $newBeam;
            break;
        case VERTICAL_SPLITTER:
            if ($beam->dir == EAST || $beam->dir == WEST) {
                $newDir1 = $beam->dir->rotate90();
                $newBeam1 = new Beam($beam->origin->add($newDir1), $newDir1);
                $newDir2 = $beam->dir->rotateNeg90();
                $newBeam2 = new Beam($beam->origin->add($newDir2), $newDir2);
                $beams[] = $newBeam1;
                $beams[] = $newBeam2;
            } else {
                $newBeam = new Beam($beam->origin->add($beam->dir), $beam->dir);
                $beams[] = $newBeam;
            }
            break;
        case HORIZONTAL_SPLITTER:
            if ($beam->dir == NORTH || $beam->dir == SOUTH) {
                $newDir1 = $beam->dir->rotate90();
                $newBeam1 = new Beam($beam->origin->add($newDir1), $newDir1);
                $newDir2 = $beam->dir->rotateNeg90();
                $newBeam2 = new Beam($beam->origin->add($newDir2), $newDir2);
                $beams[] = $newBeam1;
                $beams[] = $newBeam2;
            } else {
                $newBeam = new Beam($beam->origin->add($beam->dir), $beam->dir);
                $beams[] = $newBeam;
            }
            break;
        default:
            $newBeam = new Beam($beam->origin->add($beam->dir), $beam->dir);
            $beams[] = $newBeam;
    }

    return $beams;
}

function calculatePropagation(Grid $grid, Beam $start): array {
    $alreadySeen = [];
    $toExplore = [$start];

    while (!empty($toExplore)) {
        $beam = array_shift($toExplore);
        $key = serialize($beam);

        if ($beam->origin->isInBounds($grid) && !isset($alreadySeen[$key])) {
            $alreadySeen[$key] = true;
            $toExplore = array_merge($toExplore, nextBeam($grid, $beam));
        }
    }

    return $alreadySeen;
}

function calculateEnergization(array $alreadySeen): array {
    $alreadyEnergized = [];

    foreach ($alreadySeen as $key => $_) {
        $beam = unserialize($key);
        $coordKey = serialize($beam->origin);
        if (!isset($alreadyEnergized[$coordKey])) {
            $alreadyEnergized[$coordKey] = true;
        }
    }

    return $alreadyEnergized;
}

function solve(array $input): int {
    $grid = buildGrid($input);
    $start = new Beam(new Coord(0, 0), EAST);

    $alreadySeen = calculatePropagation($grid, $start);
    $alreadyEnergized = calculateEnergization($alreadySeen);

    return count($alreadyEnergized);
}

$input = explode("\n", trim(file_get_contents("input.txt")));
echo solve($input) . PHP_EOL;
