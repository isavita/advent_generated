
<?php

class Coord {
    public int $x;
    public int $y;

    public function __construct(int $x, int $y) {
        $this->x = $x;
        $this->y = $y;
    }

    public function neighbors(): array {
        $n = [
            new Coord($this->x + 1, $this->y),
            new Coord($this->x, $this->y + 1),
        ];

        if ($this->x > 0) {
            $n[] = new Coord($this->x - 1, $this->y);
        }
        if ($this->y > 0) {
            $n[] = new Coord($this->x, $this->y - 1);
        }

        return $n;
    }
}

const GEOLOGIC_Y = 16807;
const GEOLOGIC_X = 48271;
const CAVE_MODULO = 20183;

const TYPE_ROCKY = 0;
const TYPE_WET = 1;
const TYPE_NARROW = 2;

const TOOL_NONE = 1;
const TOOL_TORCH = 2;
const TOOL_GEAR = 4;

class Map {
    public Coord $target;
    public int $depth;
    private array $geologicIndicesCache = [];
    private array $erosionLevelsCache = [];

    public function __construct(string $input) {
        preg_match('/depth: (\d+)\ntarget: (\d+),(\d+)/', $input, $matches);
        $this->depth = (int) $matches[1];
        $this->target = new Coord((int) $matches[2], (int) $matches[3]);
    }

    public function geologicIndex(int $x, int $y): int {
        if (isset($this->geologicIndicesCache[$y][$x])) {
            return $this->geologicIndicesCache[$y][$x];
        }

        if (!isset($this->geologicIndicesCache[$y])) {
            $this->geologicIndicesCache[$y] = [];
        }

        if (($x === 0 && $y === 0) || ($x === $this->target->x && $y === $this->target->y)) {
            $this->geologicIndicesCache[$y][$x] = 0;
        } elseif ($y === 0) {
            $this->geologicIndicesCache[$y][$x] = $x * GEOLOGIC_Y;
        } elseif ($x === 0) {
            $this->geologicIndicesCache[$y][$x] = $y * GEOLOGIC_X;
        } else {
            $this->geologicIndicesCache[$y][$x] = $this->erosionLevel($x - 1, $y) * $this->erosionLevel($x, $y - 1);
        }

        return $this->geologicIndicesCache[$y][$x];
    }

    public function erosionLevel(int $x, int $y): int {
        if (isset($this->erosionLevelsCache[$y][$x])) {
            return $this->erosionLevelsCache[$y][$x];
        }

        if (!isset($this->erosionLevelsCache[$y])) {
            $this->erosionLevelsCache[$y] = [];
        }

        $this->erosionLevelsCache[$y][$x] = ($this->geologicIndex($x, $y) + $this->depth) % CAVE_MODULO;
        return $this->erosionLevelsCache[$y][$x];
    }

    public function type(int $x, int $y): int {
        return $this->erosionLevel($x, $y) % 3;
    }

    public function neighbors(Coord $pos, int $equip): array {
        $n = [];
        foreach ($pos->neighbors() as $c) {
            $t = $this->type($c->x, $c->y);
            $allowed = $this->allowed($t);
            if (($equip & $allowed) !== 0) {
                $n[] = new Item($c, $equip, 1);
                $n[] = new Item($c, $equip ^ $allowed, 8);
            }
        }
        return $n;
    }

    private function allowed(int $regionType): int {
        return match ($regionType) {
            TYPE_ROCKY => TOOL_GEAR | TOOL_TORCH,
            TYPE_WET => TOOL_GEAR | TOOL_NONE,
            TYPE_NARROW => TOOL_TORCH | TOOL_NONE,
            default => throw new Exception("unknown region type: $regionType"),
        };
    }
}

class Item {
    public Coord $pos;
    public int $equip;
    public int $time;
    public int $index = 0;

    public function __construct(Coord $pos, int $equip, int $time) {
        $this->pos = $pos;
        $this->equip = $equip;
        $this->time = $time;
    }
}

class PriorityQueue {
    private array $heap = [];

    public function __construct(array $items = []) {
        $this->heap = $items;
        $this->buildHeap();
    }

    public function isEmpty(): bool {
        return empty($this->heap);
    }

    public function push(Item $item): void {
        $item->index = count($this->heap);
        $this->heap[] = $item;
        $this->siftUp($item->index);
    }

    public function pop(): ?Item {
        if (empty($this->heap)) {
            return null;
        }
        $lastIndex = count($this->heap) - 1;
        $this->swap(0, $lastIndex);
        $item = array_pop($this->heap);
        $this->siftDown(0);
        return $item;
    }

    private function buildHeap(): void {
        $n = count($this->heap);
        for ($i = (int)floor($n / 2); $i >= 0; $i--) {
            $this->siftDown($i);
        }
    }

    private function siftUp(int $i): void {
        while ($i > 0) {
            $parent = (int)floor(($i - 1) / 2);
            if ($this->compare($this->heap[$i], $this->heap[$parent]) >= 0) {
                break;
            }
            $this->swap($i, $parent);
            $i = $parent;
        }
    }

    private function siftDown(int $i): void {
        $n = count($this->heap);
        while (true) {
            $smallest = $i;
            $left = 2 * $i + 1;
            $right = 2 * $i + 2;

            if ($left < $n && $this->compare($this->heap[$left], $this->heap[$smallest]) < 0) {
                $smallest = $left;
            }

            if ($right < $n && $this->compare($this->heap[$right], $this->heap[$smallest]) < 0) {
                $smallest = $right;
            }

            if ($smallest === $i) {
                break;
            }

            $this->swap($i, $smallest);
            $i = $smallest;
        }
    }

    private function compare(Item $a, Item $b): int {
        return $a->time <=> $b->time;
    }

    private function swap(int $i, int $j): void {
        [$this->heap[$i], $this->heap[$j]] = [$this->heap[$j], $this->heap[$i]];
        $this->heap[$i]->index = $i;
        $this->heap[$j]->index = $j;
    }
}

const BAIL_FACTOR = 8;

function rescue(string $input): int {
    $m = new Map($input);

    $queue = new PriorityQueue([new Item(new Coord(0, 0), TOOL_TORCH, 0)]);

    $distances = [];
    $distances[serialize([new Coord(0, 0), TOOL_TORCH])] = 0;

    while (!$queue->isEmpty()) {
        $item = $queue->pop();

        if ($item->pos->x === $m->target->x && $item->pos->y === $m->target->y && $item->equip === TOOL_TORCH) {
            return $item->time;
        }

        if ($item->pos->x > BAIL_FACTOR * $m->target->x || $item->pos->y > BAIL_FACTOR * $m->target->y) {
            continue;
        }

        $key = serialize([$item->pos, $item->equip]);
        if (isset($distances[$key]) && $distances[$key] < $item->time) {
            continue;
        }

        foreach ($m->neighbors($item->pos, $item->equip) as $n) {
            $d = serialize([$n->pos, $n->equip]);
            if (!isset($distances[$d]) || $item->time + $n->time < $distances[$d]) {
                $distances[$d] = $item->time + $n->time;
                $queue->push(new Item($n->pos, $n->equip, $item->time + $n->time));
            }
        }
    }

    return 0;
}

$input = file_get_contents("input.txt");
echo rescue($input) . PHP_EOL;
