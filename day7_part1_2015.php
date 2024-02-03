
<?php

$input = file_get_contents("input.txt");
$input = trim($input);

$wireToRule = [];

foreach (explode("\n", $input) as $inst) {
    $parts = explode(" -> ", $inst);
    $wireToRule[$parts[1]] = $parts[0];
}

echo someAssemblyRequired($wireToRule);

function someAssemblyRequired($wireToRule) {
    $memo = [];
    return memoDFS($wireToRule, "a", $memo);
}

function memoDFS($graph, $entry, &$memo) {
    if (array_key_exists($entry, $memo)) {
        return $memo[$entry];
    }

    if (preg_match("/[0-9]/", $entry)) {
        return (int)$entry;
    }

    $sourceRule = $graph[$entry];
    $parts = explode(" ", $sourceRule);

    $result = 0;
    switch (count($parts)) {
        case 1:
            $result = memoDFS($graph, $parts[0], $memo);
            break;
        case $parts[0] == "NOT":
            $start = memoDFS($graph, $parts[1], $memo);
            $result = (pow(2, 16) - 1) ^ $start;
            break;
        case $parts[1] == "AND":
            $result = memoDFS($graph, $parts[0], $memo) & memoDFS($graph, $parts[2], $memo);
            break;
        case $parts[1] == "OR":
            $result = memoDFS($graph, $parts[0], $memo) | memoDFS($graph, $parts[2], $memo);
            break;
        case $parts[1] == "LSHIFT":
            $result = memoDFS($graph, $parts[0], $memo) << memoDFS($graph, $parts[2], $memo);
            break;
        case $parts[1] == "RSHIFT":
            $result = memoDFS($graph, $parts[0], $memo) >> memoDFS($graph, $parts[2], $memo);
            break;
    }

    $memo[$entry] = $result;
    return $result;
}

function toInt($s) {
    return (int)$s;
}
