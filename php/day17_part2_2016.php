
<?php

$passcode = trim(file_get_contents("input.txt"));
$longestPathLength = findLongestPathLength($passcode);
echo $longestPathLength . PHP_EOL;

function findLongestPathLength($passcode) {
    $longest = 0;
    $queue = [new Point(0, 0, "")];
    while (!empty($queue)) {
        $point = array_shift($queue);

        if ($point->x == 3 && $point->y == 3) {
            if (strlen($point->path) > $longest) {
                $longest = strlen($point->path);
            }
            continue;
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
    return $longest;
}

function getOpenDoors($passcode, $path) {
    $hash = md5Hash($passcode . $path);
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

function md5Hash($input) {
    return md5($input);
}

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
?>
