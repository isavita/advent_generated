<?php

class RangeMap {
    public $srcStart, $destStart, $length;

    public function __construct($srcStart, $destStart, $length) {
        $this->srcStart = $srcStart;
        $this->destStart = $destStart;
        $this->length = $length;
    }
}

function reverseConvertNumber($number, $ranges) {
    for ($i = count($ranges) - 1; $i >= 0; $i--) {
        $r = $ranges[$i];
        if ($number >= $r->destStart && $number < $r->destStart + $r->length) {
            return $r->srcStart + ($number - $r->destStart);
        }
    }
    return $number;
}

function isInSeedRanges($number, $ranges) {
    foreach ($ranges as $r) {
        if ($number >= $r[0] && $number < $r[0] + $r[1]) {
            return true;
        }
    }
    return false;
}

$file = fopen("input.txt", "r");
if (!$file) {
    throw new Exception("Failed to open file");
}

$seedRanges = [];
$currentRanges = [];
$maps = [];

while (($line = fgets($file)) !== false) {
    $line = trim($line);
    if (strpos($line, "map:") !== false) {
        if (count($currentRanges) > 0) {
            $maps[] = $currentRanges;
            $currentRanges = [];
        }
    } elseif (strpos($line, "seeds:") === 0) {
        $seedStrs = explode(" ", substr($line, 7));
        for ($i = 0; $i < count($seedStrs); $i += 2) {
            $start = intval($seedStrs[$i]);
            $length = intval($seedStrs[$i + 1]);
            $seedRanges[] = [$start, $length];
        }
    } else {
        $numbers = preg_split('/\s+/', $line);
        if (count($numbers) == 3) {
            $srcStart = intval($numbers[1]);
            $destStart = intval($numbers[0]);
            $length = intval($numbers[2]);

            $currentRanges[] = new RangeMap($srcStart, $destStart, $length);
        }
    }
}
if (count($currentRanges) > 0) {
    $maps[] = $currentRanges;
}

fclose($file);

// Finding the lowest location number
$location = 0;
while (true) {
    $seed = $location;
    for ($i = count($maps) - 1; $i >= 0; $i--) {
        $seed = reverseConvertNumber($seed, $maps[$i]);
    }

    if (isInSeedRanges($seed, $seedRanges)) {
        echo $location . PHP_EOL;
        break;
    }
    $location++;
}

?>