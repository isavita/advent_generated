
<?php

class Point {
    public $x;
    public $y;
    public $path;

    public function __construct($x, $y, $path) {
        $this->x = $x;
        $this->y = $y;
        $this->path = $path;
    }
}

function readPasscode($filename) {
    $file = fopen($filename, "r");
    if ($file) {
        $passcode = fgets($file);
        fclose($file);
        return $passcode;
    } else {
        die("Failed to read passcode");
    }
}

function findShortestPath($passcode) {
    $queue = [new Point(0, 0, "")];
    while (count($queue) > 0) {
        $point = array_shift($queue);

        if ($point->x == 3 && $point->y == 3) {
            return $point->path;
        }

        foreach (getOpenDoors($passcode, $point->path) as $dir) {
            $nextPoint = new Point($point->x, $point->y, $point->path . $dir);
            switch ($dir) {
                case "U":
                    $nextPoint->y--;
                    break;
                case "D":
                    $nextPoint->y++;
                    break;
                case "L":
                    $nextPoint->x--;
                    break;
                case "R":
                    $nextPoint->x++;
                    break;
            }

            if ($nextPoint->x >= 0 && $nextPoint->x < 4 && $nextPoint->y >= 0 && $nextPoint->y < 4) {
                $queue[] = $nextPoint;
            }
        }
    }
    return "No path found";
}

function getOpenDoors($passcode, $path) {
    $hash = md5($passcode . $path);
    $doors = [];
    if ($hash[0] >= 'b' && $hash[0] <= 'f') {
        $doors[] = "U";
    }
    if ($hash[1] >= 'b' && $hash[1] <= 'f') {
        $doors[] = "D";
    }
    if ($hash[2] >= 'b' && $hash[2] <= 'f') {
        $doors[] = "L";
    }
    if ($hash[3] >= 'b' && $hash[3] <= 'f') {
        $doors[] = "R";
    }
    return $doors;
}

$passcode = readPasscode("input.txt");
$path = findShortestPath($passcode);
echo $path . "\n";

?>
