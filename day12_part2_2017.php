
<?php

$file = fopen("input.txt", "r");
if ($file) {
    $adj = [];
    while (($line = fgets($file)) !== false) {
        $parts = explode(" <-> ", $line);
        $from = (int)$parts[0];
        $toNodes = explode(", ", $parts[1]);

        foreach ($toNodes as $toNode) {
            $to = (int)$toNode;
            $adj[$from][] = $to;
            $adj[$to][] = $from;
        }
    }
    fclose($file);

    $visited = [];
    $groups = 0;

    foreach (array_keys($adj) as $node) {
        if (!isset($visited[$node])) {
            DFS($node, $adj, $visited);
            $groups++;
        }
    }

    echo $groups . PHP_EOL;
}

function DFS($node, &$adj, &$visited) {
    $visited[$node] = true;
    foreach ($adj[$node] as $neighbor) {
        if (!isset($visited[$neighbor])) {
            DFS($neighbor, $adj, $visited);
        }
    }
}
