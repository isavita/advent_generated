<?php

class Cave {
    private $connections = [];

    public function connectTo($name) {
        $this->connections[$name] = true;
    }

    public function disconnectFrom($name) {
        unset($this->connections[$name]);
    }

    public function getConnections() {
        return $this->connections;
    }
}

function dfs($current, $visited, &$count, $caves) {
    if ($current == "end") {
        $count++;
        return;
    }

    foreach ($caves[$current]->getConnections() as $next => $_ ) {
        if (isset($visited[$next]) && ctype_lower($next)) {
            continue;
        }

        $visitedCopy = $visited;
        $visitedCopy[$next] = true;
        dfs($next, $visitedCopy, $count, $caves);
    }
}

$caves = [];
$file = fopen("input.txt", "r");
while ($line = fgets($file)) {
    $paths = explode("-", trim($line));
    $from = $paths[0];
    $to = $paths[1];

    if (!isset($caves[$from])) {
        $caves[$from] = new Cave();
    }

    if (!isset($caves[$to])) {
        $caves[$to] = new Cave();
    }

    $caves[$from]->connectTo($to);
    $caves[$to]->connectTo($from);
}
fclose($file);

$count = 0;
dfs("start", ["start" => true], $count, $caves);
echo $count . "\n";