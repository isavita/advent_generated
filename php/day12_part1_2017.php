<?php
$file = fopen("input.txt", "r");
if (!$file) {
    echo "File reading error";
    return;
}

$adj = array();
while (($line = fgets($file)) !== false) {
    $parts = explode(" <-> ", trim($line));
    $from = intval($parts[0]);
    $toNodes = explode(", ", $parts[1]);

    foreach ($toNodes as $toNode) {
        $to = intval($toNode);
        if (!isset($adj[$from])) {
            $adj[$from] = array();
        }
        $adj[$from][] = $to;
        if (!isset($adj[$to])) {
            $adj[$to] = array();
        }
        $adj[$to][] = $from;
    }
}
fclose($file);

$visited = array();
$count = 0;

function DFS($node, $adj, &$visited) {
    $visited[$node] = true;
    if (isset($adj[$node])) {
        foreach ($adj[$node] as $neighbor) {
            if (!isset($visited[$neighbor])) {
                DFS($neighbor, $adj, $visited);
            }
        }
    }
}

DFS(0, $adj, $visited);

foreach ($visited as $v) {
    if ($v) {
        $count++;
    }
}

echo $count;