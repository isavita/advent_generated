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
                } elseif ($bricks[$j]->maxi->z > $supportZ) {
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
        $fallingBricks = [];
        foreach ($brick->support as $supportedBrick) {
            if (count($supportedBrick->basedOn) == 1) {
                $allSupportedBricks = [$supportedBrick];
                while (count($allSupportedBricks) > 0) {
                    $supportedBrick0 = array_shift($allSupportedBricks);

                    $isFalling = true;
                    foreach ($supportedBrick0->basedOn as $basedBrick) {
                        if (!array_key_exists(spl_object_id($basedBrick), $fallingBricks) && $basedBrick !== $brick) {
                            $isFalling = false;
                            break;
                        }
                    }

                    if ($isFalling) {
                        $fallingBricks[spl_object_id($supportedBrick0)] = true;
                        $allSupportedBricks = array_merge($allSupportedBricks, $supportedBrick0->support);
                    }
                }
            }
        }
        $cnt += count($fallingBricks);
    }
    return $cnt;
}

function readInputFile($fileName) {
    $fileContent = file_get_contents($fileName);
    return explode("\n", trim($fileContent));
}

$input = readInputFile("input.txt");
echo solve($input), "\n";