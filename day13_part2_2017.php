
<?php

$file = fopen("input.txt", "r");
if ($file === false) {
    echo "Error reading file\n";
    return;
}

$firewall = [];
while (($line = fgets($file)) !== false) {
    $fields = explode(": ", $line);
    $depth = (int)$fields[0];
    $range = (int)$fields[1];
    $firewall[$depth] = ["Range" => $range, "Position" => 0, "Direction" => 1];
}

$delay = 0;
while (true) {
    if (passThrough($firewall, $delay)) {
        break;
    }
    $delay++;
}

echo $delay . "\n";

function passThrough($firewall, $delay) {
    foreach ($firewall as $depth => $scanner) {
        if (($depth + $delay) % (2 * ($scanner["Range"] - 1)) == 0) {
            return false;
        }
    }
    return true;
}
?>
