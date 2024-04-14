<?php

function main() {
    $input = trim(file_get_contents("input.txt"));
    echo someAssemblyRequired($input);
}

function someAssemblyRequired($input) {
    $wireToRule = [];

    foreach (explode("\n", $input) as $inst) {
        $parts = explode(" -> ", $inst);
        $wireToRule[$parts[1]] = $parts[0];
    }

    $memo = []; // Initialize memoization array
    $aSignal = memoDFS($wireToRule, "a", $memo);
    $wireToRule["b"] = (string)$aSignal;
    $memo = []; // Reset memoization array for the second computation
    return memoDFS($wireToRule, "a", $memo);
}

function memoDFS($graph, $entry, &$memo) {
    if (isset($memo[$entry])) {
        return $memo[$entry];
    }

    if (preg_match("/^[0-9]+$/", $entry)) {
        return (int)$entry;
    }

    $sourceRule = $graph[$entry];
    $parts = explode(" ", $sourceRule);

    $result = 0;
    switch (count($parts)) {
        case 1:
            $result = memoDFS($graph, $parts[0], $memo);
            break;
        case 2:
            $start = memoDFS($graph, $parts[1], $memo);
            $result = ~$start & 0xFFFF;
            break;
        case 3:
            $left = memoDFS($graph, $parts[0], $memo);
            $right = memoDFS($graph, $parts[2], $memo);
            switch ($parts[1]) {
                case "AND":
                    $result = $left & $right;
                    break;
                case "OR":
                    $result = $left | $right;
                    break;
                case "LSHIFT":
                    $result = $left << $right;
                    break;
                case "RSHIFT":
                    $result = $left >> $right;
                    break;
            }
            break;
    }

    $memo[$entry] = $result;
    return $result;
}

main();