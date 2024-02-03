
<?php

$file = fopen("input.txt", "r");
if ($file === false) {
    echo "Error opening file\n";
    exit;
}

$time = 0;
$distance = 0;

while (($line = fgets($file)) !== false) {
    $parts = explode(":", $line);
    $line = str_replace(" ", "", $parts[1]);
    if ($time == 0) {
        $time = intval($line);
    } else {
        $distance = intval($line);
    }
}

fclose($file);

function calculateWaysToWinLongRace($time, $record) {
    $waysToWin = 0;
    for ($holdTime = 1; $holdTime < $time; $holdTime++) {
        $travelTime = $time - $holdTime;
        $distance = $holdTime * $travelTime;
        if ($distance > $record) {
            $waysToWin++;
        }
    }
    return $waysToWin;
}

$waysToWin = calculateWaysToWinLongRace($time, $distance);

echo $waysToWin . "\n";
?>
