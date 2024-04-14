<?php

class Valve {
    public $id;
    public $flow;
    public $tunnels = [];

    public function __construct($id, $flow) {
        $this->id = $id;
        $this->flow = $flow;
    }
}

function readAll($path) {
    return trim(file_get_contents($path));
}

function maxPressure($valves, $curr, $minute, $pressure, $open, $d) {
    $max = $pressure;
    foreach ($open as $next) {
        $newopen = array_filter($open, function($v) use ($next) { return $v != $next; });
        $timeLeft = $minute - $valves[$curr]->tunnels[$next] - 1;
        if ($timeLeft > 0) {
            $max = max($max, maxPressure($valves, $next, $timeLeft, $timeLeft * $valves[$next]->flow + $pressure, $newopen, $d + 1));
        }
    }
    return $max;
}

function divide($l) {
    if ($l == 1) {
        return [
            [[], [0]],
            [[0], []],
        ];
    }
    $d = divide($l - 1);
    $r = [];
    foreach ($d as $value) {
        $r[] = [array_merge([$l - 1], $value[0]), $value[1]];
        $r[] = [$value[0], array_merge([$l - 1], $value[1])];
    }
    return $r;
}

$valves = [];

$input = readAll("input.txt");
foreach (explode("\n", $input) as $line) {
    $sp = explode("; ", $line);
    sscanf($sp[0], "Valve %s has flow rate=%d", $id, $flow);
    $id = trim($id);
    $v = new Valve($id, $flow);
    $tunnels = substr($sp[1], strlen("tunnel leads to valve"));
    $tunnels = ltrim($tunnels, "s ");
    foreach (explode(", ", $tunnels) as $t) {
        $v->tunnels[$t] = 1;
    }
    $valves[$id] = $v;
}

foreach (array_keys($valves) as $k) {
    foreach (array_keys($valves) as $i) {
        foreach (array_keys($valves) as $j) {
            $dik = $valves[$i]->tunnels[$k] ?? null;
            $dkj = $valves[$k]->tunnels[$j] ?? null;
            if ($dik !== null && $dkj !== null) {
                $dij = $valves[$i]->tunnels[$j] ?? PHP_INT_MAX;
                if ($dij > $dik + $dkj) {
                    $valves[$i]->tunnels[$j] = $dik + $dkj;
                }
            }
        }
    }
}

$open = [];
foreach ($valves as $v) {
    if ($v->flow > 0) {
        $open[] = $v->id;
    }
}

$max = 0;
foreach (divide(count($open)) as $d) {
    if (count($d[0]) == 0 || count($d[1]) == 0) continue;
    $mine = array_map(function($i) use ($open) { return $open[$i]; }, $d[0]);
    $elephant = array_map(function($i) use ($open) { return $open[$i]; }, $d[1]);

    $x = maxPressure($valves, "AA", 26, 0, $mine, 0) + maxPressure($valves, "AA", 26, 0, $elephant, 0);
    $max = max($max, $x);
}

echo $max, "\n";