
<?php

$distances = readAndParseInput("input.txt");
$locations = getUniqueLocations($distances);
$maxDistance = findLongestRoute($locations, $distances);
echo $maxDistance . PHP_EOL;

function readAndParseInput($filename) {
    $file = fopen($filename, "r");
    $distances = [];

    while (!feof($file)) {
        $parts = explode(" ", fgets($file));
        if (count($parts) != 5) {
            continue; // Invalid line format
        }

        $from = $parts[0];
        $to = $parts[2];
        $dist = $parts[4];
        $distance = intval($dist);

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

function findLongestRoute($locations, $distances) {
    $maxDistance = 0;
    permute($locations, 0, $maxDistance, $distances, false);
    return $maxDistance;
}

function permute($arr, $i, &$bestDistance, $distances, $findShortest) {
    if ($i > count($arr)) {
        return;
    }
    if ($i == count($arr)) {
        $dist = calculateRouteDistance($arr, $distances);
        if ($findShortest) {
            if ($bestDistance == 0 || $dist < $bestDistance) {
                $bestDistance = $dist;
            }
        } else {
            if ($dist > $bestDistance) {
                $bestDistance = $dist;
            }
        }
        return;
    }
    for ($j = $i; $j < count($arr); $j++) {
        $temp = $arr[$i];
        $arr[$i] = $arr[$j];
        $arr[$j] = $temp;
        permute($arr, $i + 1, $bestDistance, $distances, $findShortest);
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
