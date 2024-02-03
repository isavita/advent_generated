
<?php

$input = file_get_contents("input.txt");
$targetSue = [
    "children" => 3,
    "cats" => 7,
    "samoyeds" => 2,
    "pomeranians" => 3,
    "akitas" => 0,
    "vizslas" => 0,
    "goldfish" => 5,
    "trees" => 3,
    "cars" => 2,
    "perfumes" => 1,
];

foreach (explode("\n", $input) as $line) {
    sscanf($line, "Sue %d: %s %d, %s %d, %s %d", $sueNum, $thing1, $amount1, $thing2, $amount2, $thing3, $amount3);
    $thing1 = rtrim($thing1, ":");
    $thing2 = rtrim($thing2, ":");
    $thing3 = rtrim($thing3, ":");

    $readingsMap = [
        $thing1 => $amount1,
        $thing2 => $amount2,
        $thing3 => $amount3,
    ];

    $allRulesMatched = true;

    foreach (["cats", "trees"] as $check) {
        if (array_key_exists($check, $readingsMap)) {
            if ($readingsMap[$check] <= $targetSue[$check]) {
                $allRulesMatched = false;
            }
            unset($readingsMap[$check]);
        }
    }

    foreach (["pomeranians", "goldfish"] as $check) {
        if (array_key_exists($check, $readingsMap)) {
            if ($readingsMap[$check] >= $targetSue[$check]) {
                $allRulesMatched = false;
            }
            unset($readingsMap[$check]);
        }
    }

    foreach ($readingsMap as $thing => $amount) {
        if ($targetSue[$thing] !== $amount) {
            $allRulesMatched = false;
        }
    }

    if ($allRulesMatched) {
        echo $sueNum;
        break;
    }
}
