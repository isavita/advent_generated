
<?php

function parse(): array {
    $map = [];
    $elves = [];
    $file = fopen("input.txt", "r");
    $row = 0;
    while (($line = fgets($file)) !== false) {
        $line = rtrim($line, "\r\n");
        for ($col = 0; $col < strlen($line); $col++) {
            if ($line[$col] === '#') {
                $p = ['x' => $row, 'y' => $col];
                $map[$row . "," . $col] = true;
                $elves[] = ['pos' => $p, 'moving' => false, 'nextPos' => null];
            }
        }
        $row++;
    }
    fclose($file);
    return [$map, $elves];
}

function aroundAllEmpty(array $elf, array $map): bool {
    $dirs = [
        [-1, -1], [-1, 0], [-1, 1],
        [0, 1], [1, 1], [1, 0],
        [1, -1], [0, -1]
    ];
    foreach ($dirs as $d) {
        $adj = ['x' => $elf['pos']['x'] + $d[0], 'y' => $elf['pos']['y'] + $d[1]];
        if (isset($map[$adj['x'] . "," . $adj['y']])) {
            return false;
        }
    }
    return true;
}

function elfInDirection(array $elf, int $wannaGo, array $map): bool {
    $dirs = [
        [-1, -1], [-1, 0], [-1, 1],
        [0, 1], [1, 1], [1, 0],
        [1, -1], [0, -1]
    ];
    for ($j = -1; $j <= 1; $j++) {
        $dxy = $dirs[($wannaGo + $j + 8) % 8];
        $adj = ['x' => $elf['pos']['x'] + $dxy[0], 'y' => $elf['pos']['y'] + $dxy[1]];
        if (isset($map[$adj['x'] . "," . $adj['y']])) {
            return true;
        }
    }
    return false;
}

function run(array &$map, array &$elves, int &$currDir): bool {
    $proposes = [];
    $order = [1, 5, 7, 3];
    $dirs = [
        [-1, -1], [-1, 0], [-1, 1],
        [0, 1], [1, 1], [1, 0],
        [1, -1], [0, -1]
    ];
    foreach ($elves as &$elf) {
        if (aroundAllEmpty($elf, $map)) {
            continue;
        }
        for ($i = 0; $i < 4; $i++) {
            $dir = $order[($currDir + $i) % 4];
            if (elfInDirection($elf, $dir, $map)) {
                continue;
            }
            $dxy = $dirs[$dir];
            $dest = ['x' => $elf['pos']['x'] + $dxy[0], 'y' => $elf['pos']['y'] + $dxy[1]];
            $key = $dest['x'] . "," . $dest['y'];
            $proposes[$key] = ($proposes[$key] ?? 0) + 1;
            $elf['nextPos'] = $dest;
            $elf['moving'] = true;
            break;
        }
    }
    $someoneMoved = false;
    foreach ($elves as &$elf) {
        if (!$elf['moving']) {
            continue;
        }
        $key = $elf['nextPos']['x'] . "," . $elf['nextPos']['y'];
        if (($proposes[$key] ?? 0) > 1) {
            $elf['moving'] = false;
            continue;
        }
        $someoneMoved = true;
        unset($map[$elf['pos']['x'] . "," . $elf['pos']['y']]);
        $map[$key] = true;
        $elf['pos'] = $elf['nextPos'];
        $elf['moving'] = false;
    }
    $currDir = ($currDir + 1) % 4;
    return $someoneMoved;
}

[$map, $elves] = parse();
$currDir = 0;
for ($i = 0; ; $i++) {
    if (!run($map, $elves, $currDir)) {
        echo $i + 1 . PHP_EOL;
        break;
    }
}
