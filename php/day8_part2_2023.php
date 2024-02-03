
<?php
function parseLine($line) {
    $parts = explode(" = ", $line);
    $head = $parts[0];
    $childrenTrim = trim($parts[1], "()");
    $childrenParts = explode(", ", $childrenTrim);
    return array($head, array($childrenParts[0], $childrenParts[1]));
}

function gcd($a, $b) {
    while ($b != 0) {
        list($a, $b) = array($b, $a % $b);
    }
    return $a;
}

function lcm($a, $b) {
    return ($a * $b) / gcd($a, $b);
}

function lcmSlice($nums) {
    if (count($nums) == 0) {
        return 0;
    }
    $res = $nums[0];
    for ($i = 1; $i < count($nums); $i++) {
        $res = lcm($res, $nums[$i]);
    }
    return $res;
}

function solve($input) {
    $instructions = $input[0];
    $nodes = [];
    for ($i = 2; $i < count($input); $i++) {
        list($head, $children) = parseLine($input[$i]);
        $nodes[$head] = $children;
    }

    $starts = [];
    foreach ($nodes as $node => $children) {
        $lastIndex = strlen($node) - 1;
        if ($node[$lastIndex] == 'A') {
            $starts[] = $node;
        }
    }

    $steps = [];
    $instructionsLength = strlen($instructions);
    foreach ($starts as $start) {
        $steps[] = 0;
        $element = $start;
        $lastIndex = strlen($element) - 1;
        while ($element[$lastIndex] != 'Z') {
            $instruction = $instructions[$steps[count($steps) - 1] % $instructionsLength];
            if ($instruction == 'L') {
                $element = $nodes[$element][0];
            } else {
                $element = $nodes[$element][1];
            }
            $steps[count($steps) - 1]++;
        }
    }

    $res = lcmSlice($steps);
    return $res;
}

$input = explode("\n", file_get_contents("input.txt"));
echo solve($input);
?>
