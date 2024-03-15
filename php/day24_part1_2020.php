<?php
$file = fopen("input.txt", "r");

$directions = array(
    "e" => array("q" => 1, "r" => 0),
    "se" => array("q" => 0, "r" => 1),
    "sw" => array("q" => -1, "r" => 1),
    "w" => array("q" => -1, "r" => 0),
    "nw" => array("q" => 0, "r" => -1),
    "ne" => array("q" => 1, "r" => -1)
);

$blackTiles = array();

while (($line = fgets($file)) !== false) {
    $line = trim($line);
    $coord = array("q" => 0, "r" => 0);

    for ($i = 0; $i < strlen($line); $i++) {
        $dir = "";
        switch ($line[$i]) {
            case 'e':
            case 'w':
                $dir = $line[$i];
                break;
            case 'n':
            case 's':
                $dir = substr($line, $i, 2);
                $i++;
                break;
        }
        $move = $directions[$dir];
        $coord["q"] += $move["q"];
        $coord["r"] += $move["r"];
    }

    $key = $coord["q"] . "," . $coord["r"];
    $blackTiles[$key] = !isset($blackTiles[$key]) || !$blackTiles[$key];
}

fclose($file);

$count = 0;
foreach ($blackTiles as $black) {
    if ($black) {
        $count++;
    }
}
echo $count;