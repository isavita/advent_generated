
<?php

function readRules($lines) {
    $rules = [];
    foreach ($lines as $line) {
        if ($line === "") {
            break;
        }
        $parts = explode(": ", $line);
        $rules[(int)$parts[0]] = str_replace("\"", "", $parts[1]);
    }
    return $rules;
}

function constructPattern($rules, $index) {
    if (strpos($rules[$index], "|") !== false) {
        $subrules = explode(" | ", $rules[$index]);
        $parts = [];
        foreach ($subrules as $subrule) {
            $parts[] = constructSubPattern($rules, $subrule);
        }
        return "(" . implode("|", $parts) . ")";
    }
    return constructSubPattern($rules, $rules[$index]);
}

function constructSubPattern($rules, $subrule) {
    if ($subrule === "a" || $subrule === "b") {
        return $subrule;
    }
    $subIdxs = explode(" ", $subrule);
    $pattern = "";
    foreach ($subIdxs as $idx) {
        $pattern .= constructPattern($rules, (int)$idx);
    }
    return $pattern;
}

function countMatches($lines, $pattern) {
    $count = 0;
    $re = "/^" . $pattern . "$/";
    foreach ($lines as $line) {
        if (preg_match($re, $line)) {
            $count++;
        }
    }
    return $count;
}

$input = file("input.txt", FILE_IGNORE_NEW_LINES);
$rules = readRules($input);
$pattern = constructPattern($rules, 0);
$count = countMatches($input, $pattern);

echo "The number of messages that completely match rule 0 is: " . $count . PHP_EOL;
?>
