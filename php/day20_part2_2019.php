
<?php

define('WALL', '#');
define('FREE', '.');

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
    
    public function __toString(): string {
        return "{$this->x},{$this->y}";
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
    $map = new MapData();
    $file = fopen("input.txt", "r");
    if (!$file) {
        throw new Exception("Failed to open input.txt");
    }

    $i = 0;
    while (($line = fgets($file)) !== false) {
        $line = rtrim($line, "\r\n");
        $yMax = strlen($line);
        if ($yMax > $map->yMax) {
            $map->yMax = $yMax;
        }
        for ($j = 0; $j < $yMax; $j++) {
            $map->grid[(string)new P($i, $j)] = $line[$j];
        }
        $i++;
    }
    fclose($file);
    $map->xMax = $i;

    $cache = [];

    for ($i = 0; $i < $map->xMax; $i++) {
        for ($j = 0; $j < $map->yMax; $j++) {
            $p = new P($i, $j);
            $c = $map->grid[(string)$p] ?? null;
            if (!ctype_upper($c)) {
                continue;
            }
            
            $portalData = extractPortal($map->grid, $p);
            if (!$portalData['ok']) {
                continue;
            }
            
            $pName = $portalData['name'];
            $pPoint = $portalData['point'];
            
            $map->portalName[(string)$pPoint] = $pName;

            if ($pName === "AA") {
                $map->aa = $pPoint;
                $map->isOuter[(string)$pPoint] = true;
                continue;
            }

            if ($pName === "ZZ") {
                $map->zz = $pPoint;
                $map->isOuter[(string)$pPoint] = true;
                continue;
            }

            if (isset($cache[$pName])) {
                $map->teleport[(string)$pPoint] = $cache[$pName];
                $map->teleport[(string)$cache[$pName]] = $pPoint;
            } else {
                $cache[$pName] = $pPoint;
            }

            if ($j === 0 || $i === 0 || $i === $map->xMax - 2 || $j === $map->yMax - 2) {
                $map->isOuter[(string)$pPoint] = true;
            } else {
                $map->isOuter[(string)$pPoint] = false;
            }
        }
    }
    return $map;
}

function extractPortal(array $grid, P $p): array {
    $c1 = $grid[(string)$p] ?? null;
    
    $c2 = $grid[(string)new P($p->x + 1, $p->y)] ?? null;
    if (ctype_upper($c2)) {
        $portalName = $c1 . $c2;
        $portalPoint = new P($p->x + 2, $p->y);
        if (($grid[(string)$portalPoint] ?? null) === FREE) {
            return ['name' => $portalName, 'point' => $portalPoint, 'ok' => true];
        }
        $portalPoint = new P($p->x - 1, $p->y);
        if (($grid[(string)$portalPoint] ?? null) === FREE) {
            return ['name' => $portalName, 'point' => $portalPoint, 'ok' => true];
        }
    }

    $c2 = $grid[(string)new P($p->x, $p->y + 1)] ?? null;
    if (ctype_upper($c2)) {
        $portalName = $c1 . $c2;
        $portalPoint = new P($p->x, $p->y + 2);
        if (($grid[(string)$portalPoint] ?? null) === FREE) {
            return ['name' => $portalName, 'point' => $portalPoint, 'ok' => true];
        }
        $portalPoint = new P($p->x, $p->y - 1);
        if (($grid[(string)$portalPoint] ?? null) === FREE) {
            return ['name' => $portalName, 'point' => $portalPoint, 'ok' => true];
        }
    }

    return ['name' => '', 'point' => null, 'ok' => false];
}

function BFSNested(MapData $map): int {
    $discovered = [];
    $toDo = [];

    $root = ['p' => $map->aa, 'depth' => 0];
    $discovered[(string)$map->aa . ",0"] = true;
    $toDo[] = $root;

    $steps = 0;

    while (count($toDo) > 0) {
        $levelSize = count($toDo);
        for ($i = 0; $i < $levelSize; $i++) {
            $curr = array_shift($toDo);
            
            foreach ($curr['p']->neighbours() as $n) {
                $dest = $map->grid[(string)$n] ?? null;
                
                if ($dest === WALL) {
                    continue;
                }
                
                if ($dest === FREE) {
                    $target = ['p' => $n, 'depth' => $curr['depth']];
                    if (!isset($discovered[(string)$n . "," . $curr['depth']])) {
                        $discovered[(string)$n . "," . $curr['depth']] = true;
                        $toDo[] = $target;
                    }
                } elseif (ctype_upper($dest)) {
                    $isOuter = $map->isOuter[(string)$curr['p']] ?? false;
                    
                    if (!$isOuter) {
                        $target = ['p' => $map->teleport[(string)$curr['p']], 'depth' => $curr['depth'] + 1];
                    } else {
                        $portalName = $map->portalName[(string)$curr['p']] ?? null;
                        if ($curr['depth'] === 0) {
                            if ($portalName === "ZZ") {
                                return $steps;
                            }
                            continue;
                        }
                        if ($portalName === "AA" || $portalName === "ZZ") {
                            continue;
                        }
                        $target = ['p' => $map->teleport[(string)$curr['p']], 'depth' => $curr['depth'] - 1];
                    }
                    if (!isset($discovered[(string)$target['p'] . "," . $target['depth']])) {
                        $discovered[(string)$target['p'] . "," . $target['depth']] = true;
                        $toDo[] = $target;
                    }
                }
            }
        }
        $steps++;
    }
    return -1;
}

$map = parse();
echo BFSNested($map) . PHP_EOL;
