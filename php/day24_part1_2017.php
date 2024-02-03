
<?php

$components = [];
$maxStrength = 0;

$handle = fopen("input.txt", "r");
if ($handle) {
    while (($line = fgets($handle)) !== false) {
        $ports = explode("/", $line);
        $a = (int)$ports[0];
        $b = (int)$ports[1];
        $components[] = [$a, $b];
    }

    fclose($handle);
} else {
    die("Error opening the file.");
}

function findStrongestBridge($components, $used, $port, $strength) {
    global $maxStrength;

    if ($strength > $maxStrength) {
        $maxStrength = $strength;
    }

    foreach ($components as $i => $c) {
        if ($used[$i]) {
            continue;
        }

        if ($c[0] == $port || $c[1] == $port) {
            $used[$i] = true;
            $nextPort = $c[0];
            if ($c[0] == $port) {
                $nextPort = $c[1];
            }
            findStrongestBridge($components, $used, $nextPort, $strength + $c[0] + $c[1]);
            $used[$i] = false;
        }
    }
}

$used = array_fill(0, count($components), false);
findStrongestBridge($components, $used, 0, 0);

echo $maxStrength . PHP_EOL;
