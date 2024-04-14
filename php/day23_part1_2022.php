<?php

class P {
    public $X;
    public $Y;

    public function __construct($x, $y) {
        $this->X = $x;
        $this->Y = $y;
    }
}

class Elf {
    public $Pos;
    public $Moving = false;
    public $NextPos;

    public function __construct($pos) {
        $this->Pos = $pos;
    }
}

define('N', 1);
define('E', 3);
define('S', 5);
define('W', 7);

$Map = [];
$Elves = [];
$Order = [N, S, W, E];
$CurrDir = 0;
$Dirs = [
    new P(-1, -1), // NW
    new P(-1, 0),  // N
    new P(-1, 1),  // NE
    new P(0, 1),   // E
    new P(1, 1),   // SE
    new P(1, 0),   // S
    new P(1, -1),  // SW
    new P(0, -1)   // W
];

function parse() {
    global $Map, $Elves;
    $file = fopen("input.txt", "r");
    if (!$file) throw new Exception("Unable to open file!");
    $row = 0;
    while (($line = fgets($file)) !== false) {
        $line = trim($line);
        for ($col = 0; $col < strlen($line); $col++) {
            if ($line[$col] == '#') {
                $p = new P($row, $col);
                $Map["{$p->X},{$p->Y}"] = new stdClass();
                $Elves[] = new Elf($p);
            }
        }
        $row++;
    }
    fclose($file);
}

function AroundAllEmpty($elf) {
    global $Map, $Dirs;
    foreach ($Dirs as $d) {
        $adj = new P($elf->Pos->X + $d->X, $elf->Pos->Y + $d->Y);
        if (isset($Map["{$adj->X},{$adj->Y}"])) {
            return false;
        }
    }
    return true;
}

function ElfInDirection($elf, $wannaGo) {
    global $Map, $Dirs;
    for ($j = -1; $j <= 1; $j++) {
        $dxy = $Dirs[($wannaGo + $j + 8) % 8];
        $adj = new P($elf->Pos->X + $dxy->X, $elf->Pos->Y + $dxy->Y);
        if (isset($Map["{$adj->X},{$adj->Y}"])) {
            return true;
        }
    }
    return false;
}

function run() {
    global $Elves, $Map, $Order, $CurrDir, $Dirs;
    $proposes = [];
    $someoneMoved = false;

    foreach ($Elves as $e) {
        if (AroundAllEmpty($e)) {
            continue;
        }

        for ($i = 0; $i < 4; $i++) {
            $dir = $Order[($CurrDir + $i) % 4];
            if (ElfInDirection($e, $dir)) {
                continue;
            }

            $dxy = $Dirs[$dir];
            $dest = new P($e->Pos->X + $dxy->X, $e->Pos->Y + $dxy->Y);
            $proposes["{$dest->X},{$dest->Y}"] = ($proposes["{$dest->X},{$dest->Y}"] ?? 0) + 1;
            $e->NextPos = $dest;
            $e->Moving = true;
            break;
        }
    }

    foreach ($Elves as $e) {
        if (!$e->Moving) {
            continue;
        }

        if ($proposes["{$e->NextPos->X},{$e->NextPos->Y}"] > 1) {
            $e->Moving = false;
            continue;
        }

        $someoneMoved = true;
        unset($Map["{$e->Pos->X},{$e->Pos->Y}"]);
        $Map["{$e->NextPos->X},{$e->NextPos->Y}"] = new stdClass();
        $e->Pos = $e->NextPos;
        $e->Moving = false;
    }

    $CurrDir = ($CurrDir + 1) % 4;
    return $someoneMoved;
}

function minMax() {
    global $Map;
    $min = new P(PHP_INT_MAX, PHP_INT_MAX);
    $max = new P(PHP_INT_MIN, PHP_INT_MIN);
    foreach ($Map as $key => $_) {
        list($x, $y) = explode(',', $key);
        if ($x < $min->X) $min->X = $x;
        if ($y < $min->Y) $min->Y = $y;
        if ($x > $max->X) $max->X = $x;
        if ($y > $max->Y) $max->Y = $y;
    }
    return [$min, $max];
}

parse();

for ($i = 0; $i < 10; $i++) {
    run();
}

list($min, $max) = minMax();

$count = 0;
for ($x = $min->X; $x <= $max->X; $x++) {
    for ($y = $min->Y; $y <= $max->Y; $y++) {
        if (!isset($Map["$x,$y"])) {
            $count++;
        }
    }
}

echo $count . "\n";