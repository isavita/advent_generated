<?php
function tokenize($expression) {
    $expression = str_replace(array('(', ')'), array('( ', ' )'), $expression);
    return explode(' ', $expression);
}

function evaluateTokens($tokens) {
    $ops = [];
    $vals = [];

    foreach ($tokens as $token) {
        switch ($token) {
            case '(':
                $ops[] = $token;
                break;
            case '+':
            case '*':
                while (!empty($ops) && $ops[count($ops) - 1] != '(') {
                    $val2 = array_pop($vals);
                    $val1 = array_pop($vals);
                    $op = array_pop($ops);
                    $vals[] = applyOp($op, $val1, $val2);
                }
                $ops[] = $token;
                break;
            case ')':
                while ($ops[count($ops) - 1] != '(') {
                    $val2 = array_pop($vals);
                    $val1 = array_pop($vals);
                    $op = array_pop($ops);
                    $vals[] = applyOp($op, $val1, $val2);
                }
                array_pop($ops); // Remove the opening '('
                break;
            default:
                $vals[] = intval($token);
                break;
        }
    }

    while (!empty($ops)) {
        $val2 = array_pop($vals);
        $val1 = array_pop($vals);
        $op = array_pop($ops);
        $vals[] = applyOp($op, $val1, $val2);
    }

    return $vals[0];
}

function applyOp($op, $a, $b) {
    switch ($op) {
        case '+':
            return $a + $b;
        case '*':
            return $a * $b;
        default:
            throw new Exception("Unknown operator: $op");
    }
}

$file = fopen('input.txt', 'r');
$sum = 0;
while (($line = fgets($file)) !== false) {
    $expression = trim($line);
    $result = evaluateTokens(tokenize($expression));
    $sum += $result;
}
fclose($file);

echo $sum . PHP_EOL;