
<?php

const WALL = '#';
const FREE = '.';

class P {
    public int $x;
    public int $y;

    public function __construct(int $x, int $y) {
        $this->x = $x;
        $this->y = $y;
    }

    public function neighbours(): array {
        return [
            new P($this->x, $this->y + 1),
            new P($this->x + 1, $this->y),
            new P($this->x, $this->y - 1),
            new P($this->x - 1, $this->y),
        ];
    }
}

class MapData {
    public int $xMax = 0;
    public int $yMax = 0;
    public array $grid = [];
    public ?P $aa = null;
    public ?P $zz = null;
    public array $teleport = [];
    public array $portalName = [];
    public array $isOuter = [];
}

function parse(): MapData {
    $mapData = new MapData();
    $file = fopen("input.txt", "r");
    if (!$file) {
        throw new Exception("Failed to open input.txt");
    }

    $i = 0;
    while (($line = fgets($file)) !== false) {
        $line = rtrim($line, "\r\n");
        $yMax = strlen($line);
        if ($yMax > $mapData->yMax) {
            $mapData->yMax = $yMax;
        }
        for ($j = 0; $j < $yMax; $j++) {
            $mapData->grid[($i << 16) | $j] = $line[$j];
        }
        $i++;
    }
    fclose($file);
    $mapData->xMax = $i;

    $cache = [];
    for ($i = 0; $i < $mapData->xMax; $i++) {
        for ($j = 0; $j < $mapData->yMax; $j++) {
            $c = $mapData->grid[($i << 16) | $j] ?? null;
            if ($c === null || !ctype_upper($c)) {
                continue;
            }
            $portalData = extractPortal($mapData, new P($i, $j));
            if (!$portalData['ok']) {
                continue;
            }
            $pName = $portalData['name'];
            $pPoint = $portalData['point'];
            $mapData->portalName[($pPoint->x << 16) | $pPoint->y] = $pName;

            if ($pName === "AA") {
                $mapData->aa = $pPoint;
                $mapData->isOuter[($pPoint->x << 16) | $pPoint->y] = true;
                continue;
            }
            if ($pName === "ZZ") {
                $mapData->zz = $pPoint;
                $mapData->isOuter[($pPoint->x << 16) | $pPoint->y] = true;
                continue;
            }
            if (isset($cache[$pName])) {
                $target = $cache[$pName];
                $mapData->teleport[($pPoint->x << 16) | $pPoint->y] = $target;
                $mapData->teleport[($target->x << 16) | $target->y] = $pPoint;
            } else {
                $cache[$pName] = $pPoint;
            }
            if ($j === 0 || $i === 0 || $i === $mapData->xMax - 2 || $j === $mapData->yMax - 2) {
                $mapData->isOuter[($pPoint->x << 16) | $pPoint->y] = true;
            } else {
                $mapData->isOuter[($pPoint->x << 16) | $pPoint->y] = false;
            }
        }
    }
    return $mapData;
}

function extractPortal(MapData $mapData, P $p): array {
    $c1 = $mapData->grid[($p->x << 16) | $p->y] ?? null;
    if ($c1 === null) {
        return ['name' => '', 'point' => null, 'ok' => false];
    }
    $c2 = $mapData->grid[(($p->x + 1) << 16) | $p->y] ?? null;
    if ($c2 !== null && ctype_upper($c2)) {
        $portalName = $c1 . $c2;
        $portalPoint = new P($p->x + 2, $p->y);
        if (($mapData->grid[($portalPoint->x << 16) | $portalPoint->y] ?? null) === FREE) {
            return ['name' => $portalName, 'point' => $portalPoint, 'ok' => true];
        }
        $portalPoint = new P($p->x - 1, $p->y);
        if (($mapData->grid[($portalPoint->x << 16) | $portalPoint->y] ?? null) === FREE) {
            return ['name' => $portalName, 'point' => $portalPoint, 'ok' => true];
        }
    }
    $c2 = $mapData->grid[($p->x << 16) | ($p->y + 1)] ?? null;
    if ($c2 !== null && ctype_upper($c2)) {
        $portalName = $c1 . $c2;
        $portalPoint = new P($p->x, $p->y + 2);
        if (($mapData->grid[($portalPoint->x << 16) | $portalPoint->y] ?? null) === FREE) {
            return ['name' => $portalName, 'point' => $portalPoint, 'ok' => true];
        }
        $portalPoint = new P($p->x, $p->y - 1);
        if (($mapData->grid[($portalPoint->x << 16) | $portalPoint->y] ?? null) === FREE) {
            return ['name' => $portalName, 'point' => $portalPoint, 'ok' => true];
        }
    }
    return ['name' => '', 'point' => null, 'ok' => false];
}

function bfs(MapData $mapData): int {
    $discovered = [];
    $toDo = [];
    $discovered[($mapData->aa->x << 16) | $mapData->aa->y] = true;
    $toDo[] = $mapData->aa;
    $depth = 0;
    while (count($toDo) > 0) {
        $levelSize = count($toDo);
        for ($i = 0; $i < $levelSize; $i++) {
            $curr = array_shift($toDo);
            if ($curr->x === $mapData->zz->x && $curr->y === $mapData->zz->y) {
                return $depth;
            }
            foreach ($curr->neighbours() as $n) {
                $dest = $mapData->grid[($n->x << 16) | $n->y] ?? null;
                if ($dest === WALL) {
                    continue;
                }
                if ($dest === FREE) {
                    if (!isset($discovered[($n->x << 16) | $n->y])) {
                        $discovered[($n->x << 16) | $n->y] = true;
                        $toDo[] = $n;
                    }
                } elseif (ctype_upper($dest)) {
                    $next = $mapData->teleport[($curr->x << 16) | $curr->y] ?? null;
                    if ($next !== null && !isset($discovered[($next->x << 16) | $next->y])) {
                        $discovered[($next->x << 16) | $next->y] = true;
                        $toDo[] = $next;
                    }
                }
            }
        }
        $depth++;
    }
    return -1;
}

$mapData = parse();
echo bfs($mapData) . PHP_EOL;
