<?php

class Claim {
    public $id;
    public $left;
    public $top;
    public $width;
    public $height;

    public static function parseClaim($line) {
        $parts = explode(" ", $line);
        $id = substr($parts[0], 1);
        $coords = explode(",", $parts[2]);
        $size = explode("x", $parts[3]);
        return new Claim($id, (int)$coords[0], (int)$coords[1], (int)$size[0], (int)$size[1]);
    }

    public function __construct($id, $left, $top, $width, $height) {
        $this->id = $id;
        $this->left = $left;
        $this->top = $top;
        $this->width = $width;
        $this->height = $height;
    }
}

function readClaims($filename) {
    $claims = [];
    if (($handle = fopen($filename, "r")) !== FALSE) {
        while (($line = fgets($handle)) !== FALSE) {
            $claims[] = Claim::parseClaim(trim($line));
        }
        fclose($handle);
    }
    return $claims;
}

function countOverlappingInches($claims) {
    $fabric = array_fill(0, 1000, array_fill(0, 1000, 0));
    foreach ($claims as $claim) {
        for ($i = $claim->left; $i < $claim->left + $claim->width; $i++) {
            for ($j = $claim->top; $j < $claim->top + $claim->height; $j++) {
                $fabric[$i][$j]++;
            }
        }
    }

    $overlapping = 0;
    for ($i = 0; $i < 1000; $i++) {
        for ($j = 0; $j < 1000; $j++) {
            if ($fabric[$i][$j] > 1) {
                $overlapping++;
            }
        }
    }
    return $overlapping;
}

$claims = readClaims("input.txt");
$overlapping = countOverlappingInches($claims);
echo $overlapping . "\n";