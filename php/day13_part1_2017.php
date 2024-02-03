
<?php

$file = fopen("input.txt", "r");
if ($file === false) {
    echo "Error reading file\n";
    exit;
}

$firewall = [];
while (($line = fgets($file)) !== false) {
    $fields = explode(": ", $line);
    $depth = (int)$fields[0];
    $range = (int)$fields[1];
    $firewall[$depth] = ["Range" => $range, "Position" => 0, "Direction" => 1];
}

$severity = 0;

for ($depth = 0; $depth <= maxDepth($firewall); $depth++) {
    if (array_key_exists($depth, $firewall)) {
        if ($firewall[$depth]["Position"] == 0) {
            $severity += $depth * $firewall[$depth]["Range"];
        }
    }

    foreach ($firewall as &$scanner) {
        moveScanner($scanner);
    }
}

echo $severity . "\n";

function maxDepth($firewall) {
    $max = 0;
    foreach (array_keys($firewall) as $depth) {
        if ($depth > $max) {
            $max = $depth;
        }
    }
    return $max;
}

function moveScanner(&$scanner) {
    if ($scanner["Position"] == 0) {
        $scanner["Direction"] = 1;
    } elseif ($scanner["Position"] == $scanner["Range"] - 1) {
        $scanner["Direction"] = -1;
    }
    $scanner["Position"] += $scanner["Direction"];
}
