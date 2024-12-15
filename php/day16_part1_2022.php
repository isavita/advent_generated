
<?php

function readAll(string $path): string {
    return trim(file_get_contents($path));
}

function maxPressure(array $valves, string $curr, int $minute, int $pressure, array $open, int $d): int {
    $max = $pressure;
    foreach ($open as $next) {
        $newopen = array_filter($open, function ($v) use ($next) {
            return $v !== $next;
        });
        $timeLeft = $minute - $valves[$curr]['tunnels'][$next] - 1;
        if ($timeLeft > 0) {
            $max = max($max, maxPressure($valves, $next, $timeLeft, $timeLeft * $valves[$next]['flow'] + $pressure, $newopen, $d + 1));
        }
    }
    return $max;
}

$valves = [];
$input = readAll("input.txt");
foreach (explode("\n", $input) as $line) {
    $sp = explode("; ", $line);
    preg_match("/Valve (\w+) has flow rate=(\d+)/", $sp[0], $matches);
    $v = ['id' => $matches[1], 'flow' => (int)$matches[2], 'tunnels' => [$matches[1] => 0]];
    $tunnels = substr($sp[1], strpos($sp[1], "valve") + 5);
    if (str_starts_with($tunnels, "s")) {
        $tunnels = substr($tunnels, 2);
    } else {
        $tunnels = substr($tunnels, 1);
    }
    foreach (explode(", ", $tunnels) as $t) {
        $v['tunnels'][$t] = 1;
    }
    $valves[$v['id']] = $v;
}

foreach ($valves as $k => $_) {
    foreach ($valves as $i => $_) {
        foreach ($valves as $j => $_) {
            if (isset($valves[$i]['tunnels'][$k]) && isset($valves[$k]['tunnels'][$j])) {
                $dik = $valves[$i]['tunnels'][$k];
                $dkj = $valves[$k]['tunnels'][$j];
                if (!isset($valves[$i]['tunnels'][$j]) || $valves[$i]['tunnels'][$j] > $dik + $dkj) {
                    $valves[$i]['tunnels'][$j] = $dik + $dkj;
                }
            }
        }
    }
}

$open = [];
foreach ($valves as $v) {
    if ($v['flow'] > 0) {
        $open[] = $v['id'];
    }
}

echo maxPressure($valves, "AA", 30, 0, $open, 0) . PHP_EOL;
?>
