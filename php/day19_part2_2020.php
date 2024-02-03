
<?php

$file = file_get_contents("input.txt");
$input = trim($file);
$result = solve($input);
echo $result . PHP_EOL;

function solve($input) {
    list($graph, $messages) = parseInput($input);

    fillInGraph($graph, 42);
    fillInGraph($graph, 31);

    $part42 = "(" . implode("|", $graph[42]->resolved) . ")";
    $part31 = "(" . implode("|", $graph[31]->resolved) . ")";

    $rule8String = "(" . $part42 . ")+";

    $matchRuleZero = 0;
    foreach ($messages as $m) {
        for ($i = 1; $i < 10; $i++) {
            $pattern = "/^" . $rule8String . $part42 . "{" . $i . "}" . $part31 . "{" . $i . "}$/";
            if (preg_match($pattern, $m)) {
                $matchRuleZero++;
                break;
            }
        }
    }

    return $matchRuleZero;
}

function fillInGraph(&$graph, $entry) {
    if (!empty($graph[$entry]->resolved)) {
        return $graph[$entry]->resolved;
    }

    foreach ($graph[$entry]->options as $option) {
        $resolved = [""];
        foreach ($option as $entryPoint) {
            $nestedResolveVals = fillInGraph($graph, $entryPoint);
            $newResolved = [];
            foreach ($nestedResolveVals as $nextPiece) {
                foreach ($resolved as $prev) {
                    $newResolved[] = $prev . $nextPiece;
                }
            }
            $resolved = $newResolved;
        }
        $graph[$entry]->resolved = array_merge($graph[$entry]->resolved, $resolved);
    }

    return $graph[$entry]->resolved;
}

class Rule {
    public $resolved = [];
    public $options = [];
}

function parseInput($input) {
    $parts = explode("\n\n", $input);

    $rules = [];
    foreach (explode("\n", $parts[0]) as $r) {
        if (preg_match("/[a-z]/", $r)) {
            list($num, $char) = sscanf($r, "%d: \"%1s\"");
            $rules[$num] = new Rule();
            $rules[$num]->resolved = [$char];
        } else {
            list($key, $rule) = explode(": ", $r);
            $key = intval($key);
            $newRule = new Rule();
            foreach (explode(" | ", $rule) as $ruleNums) {
                $nums = explode(" ", $ruleNums);
                $option = [];
                foreach ($nums as $n) {
                    $option[] = intval($n);
                }
                $newRule->options[] = $option;
            }
            $rules[$key] = $newRule;
        }
    }

    $messages = explode("\n", $parts[1]);

    return [$rules, $messages];
}

