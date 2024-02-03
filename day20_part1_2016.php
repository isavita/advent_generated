
<?php

$ipRanges = readIPRanges("input.txt");
usort($ipRanges, function($a, $b) {
    return $a['start'] - $b['start'];
});

$unblockedIP = findUnblockedIP($ipRanges);
echo $unblockedIP . PHP_EOL;

function readIPRanges($filename) {
    $file = fopen($filename, "r");
    if ($file === false) {
        die("Error opening file");
    }

    $ranges = [];
    while (($line = fgets($file)) !== false) {
        $parts = explode("-", $line);
        $start = (int)$parts[0];
        $end = (int)$parts[1];
        $ranges[] = ['start' => $start, 'end' => $end];
    }
    fclose($file);
    return $ranges;
}

function findUnblockedIP($ranges) {
    $currentIP = 0;
    foreach ($ranges as $r) {
        if ($r['start'] > $currentIP) {
            return $currentIP;
        }
        if ($r['end'] >= $currentIP) {
            $currentIP = $r['end'] + 1;
        }
    }
    return $currentIP;
}
?>
