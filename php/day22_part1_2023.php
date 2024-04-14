<?php

class Coord {
    public $x;
    public $y;
    public $z;

    public function __construct($x, $y, $z) {
        $this->x = $x;
        $this->y = $y;
        $this->z = $z;
    }
}

class Brick {
    public $mini;
    public $maxi;
    public $basedOn;
    public $support;

    public function __construct() {
        $this->mini = new Coord(0, 0, 0);
        $this->maxi = new Coord(0, 0, 0);
        $this->basedOn = [];
        $this->support = [];
    }
}

function parseInput($input) {
    $bricks = [];
    foreach ($input as $line) {
        $brick = new Brick();
        sscanf($line, "%d,%d,%d~%d,%d,%d", 
            $brick->mini->x, $brick->mini->y, $brick->mini->z, 
            $brick->maxi->x, $brick->maxi->y, $brick->maxi->z);
        $bricks[] = $brick;
    }
    return $bricks;
}

function settle(&$bricks) {
    usort($bricks, function($a, $b) {
        return $a->maxi->z <=> $b->maxi->z;
    });

    foreach ($bricks as $i => $brick) {
        $supportZ = 0;
        $basedBricks = [];

        for ($j = $i - 1; $j >= 0; $j--) {
            $isIntersectingX = max($brick->mini->x, $bricks[$j]->mini->x) <= min($brick->maxi->x, $bricks[$j]->maxi->x);
            $isIntersectingY = max($brick->mini->y, $bricks[$j]->mini->y) <= min($brick->maxi->y, $bricks[$j]->maxi->y);
            $isIntersecting = $isIntersectingX && $isIntersectingY;
            if ($isIntersecting) {
                if ($bricks[$j]->maxi->z == $supportZ) {
                    $basedBricks[] = $bricks[$j];
                } else if ($bricks[$j]->maxi->z > $supportZ) {
                    $supportZ = $bricks[$j]->maxi->z;
                    $basedBricks = [$bricks[$j]];
                }
            }
        }

        $brick->basedOn = $basedBricks;
        foreach ($basedBricks as $basedBrick) {
            $basedBrick->support[] = $brick;
        }

        $deltaZ = $brick->maxi->z - $brick->mini->z;
        $brick->mini->z = $supportZ + 1;
        $brick->maxi->z = $brick->mini->z + $deltaZ;
    }
}

function solve($input) {
    $bricks = parseInput($input);
    settle($bricks);

    $cnt = 0;
    foreach ($bricks as $brick) {
        $isDisintegratable = true;
        foreach ($brick->support as $supportedBrick) {
            if (count($supportedBrick->basedOn) < 2) {
                $isDisintegratable = false;
                break;
            }
        }
        if ($isDisintegratable) {
            $cnt++;
        }
    }
    return $cnt;
}

function readFileContents($fileName) {
    $fileContents = file_get_contents($fileName);
    return explode("\n", trim($fileContents));
}

$input = readFileContents("input.txt");
echo solve($input);