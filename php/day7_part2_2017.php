<?php
$data = file_get_contents("input.txt");
$lines = explode("\n", trim($data));

$programs = array();

$pattern = "/([a-z]+)|\d+/";
foreach ($lines as $line) {
    preg_match_all($pattern, $line, $matches);
    $name = $matches[0][0];
    $weight = intval($matches[0][1]);
    $program = array("Weight" => $weight);
    if (count($matches[0]) > 2) {
        $program["Holds"] = array_slice($matches[0], 2);
    }
    $programs[$name] = $program;
}

function dfs($name, $programs) {
    $program = $programs[$name];
    $totalWeight = $program["Weight"];

    $weights = array();
    if (isset($program["Holds"])) {
        foreach ($program["Holds"] as $child) {
            list($weight, $balanced) = dfs($child, $programs);
            if (!$balanced) {
                return array(0, false);
            }
            $totalWeight += $weight;
            if (!isset($weights[$weight])) {
                $weights[$weight] = 0;
            }
            $weights[$weight]++;
        }
    }

    // Check for unbalance
    foreach ($weights as $w1 => $c1) {
        foreach ($weights as $w2 => $c2) {
            if ($w1 != $w2 && $c1 < $c2) {
                $unbalancedProgram = "";
                if (isset($program["Holds"])) {
                    foreach ($program["Holds"] as $child) {
                        list($childWeight, $balanced) = dfs($child, $programs);
                        if ($childWeight == $w1) {
                            $unbalancedProgram = $child;
                            break;
                        }
                    }
                }
                echo $programs[$unbalancedProgram]["Weight"] + ($w2 - $w1) . "\n";
                return array(0, false);
            }
        }
    }
    return array($totalWeight, true);
}

$root = "dtacyn"; // Replace this with the root found in Part One
dfs($root, $programs);
?>