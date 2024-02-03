
<?php

$distances = readAndParseInput("input.txt");
$locations = getUniqueLocations($distances);
$minDistance = findShortestRoute($locations, $distances);
echo $minDistance . PHP_EOL;

function readAndParseInput($filename) {
    $file = fopen($filename, "r");
    $distances = [];

    while (!feof($file)) {
        $parts = explode(" ", trim(fgets($file)));
        if (count($parts) != 5) {
            continue; // Invalid line format
        }

        $from = $parts[0];
        $to = $parts[2];
        $dist = $parts[4];
        $distance = (int)$dist;

        if (!array_key_exists($from, $distances)) {
            $distances[$from] = [];
        }
        $distances[$from][$to] = $distance;

        if (!array_key_exists($to, $distances)) {
            $distances[$to] = [];
        }
        $distances[$to][$from] = $distance; // Assuming distance is symmetric
    }

    fclose($file);
    return $distances;
}

function getUniqueLocations($distances) {
    $locationSet = [];
    foreach ($distances as $from => $toDistances) {
        $locationSet[$from] = true;
        foreach ($toDistances as $to => $distance) {
            $locationSet[$to] = true;
        }
    }

    return array_keys($locationSet);
}

function findShortestRoute($locations, $distances) {
    $minDistance = -1;
    permute($locations, 0, $minDistance, $distances);
    return $minDistance;
}

function permute(&$arr, $i, &$minDistance, $distances) {
    if ($i > count($arr)) {
        return;
    }
    if ($i == count($arr)) {
        $dist = calculateRouteDistance($arr, $distances);
        if ($minDistance == -1 || $dist < $minDistance) {
            $minDistance = $dist;
        }
        return;
    }
    for ($j = $i; $j < count($arr); $j++) {
        $temp = $arr[$i];
        $arr[$i] = $arr[$j];
        $arr[$j] = $temp;
        permute($arr, $i + 1, $minDistance, $distances);
        $temp = $arr[$i];
        $arr[$i] = $arr[$j];
        $arr[$j] = $temp;
    }
}

function calculateRouteDistance($route, $distances) {
    $sum = 0;
    for ($i = 0; $i < count($route) - 1; $i++) {
        $sum += $distances[$route[$i]][$route[$i + 1]];
    }
    return $sum;
}
?>
