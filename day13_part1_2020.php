
<?php

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);
$earliestDeparture = (int)$lines[0];
$busIDs = explode(",", $lines[1]);

$earliestBusID = 0;
$minWaitTime = $earliestDeparture;

foreach ($busIDs as $id) {
    if ($id === "x") {
        continue;
    }
    $busID = (int)$id;
    $waitTime = $busID - ($earliestDeparture % $busID);
    if ($waitTime < $minWaitTime) {
        $minWaitTime = $waitTime;
        $earliestBusID = $busID;
    }
}

echo $earliestBusID * $minWaitTime . PHP_EOL;
?>
