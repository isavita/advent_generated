
<?php

$file = fopen("input.txt", "r");
$scores = [];

while (!feof($file)) {
    $line = trim(fgets($file));
    if ($line != "") {
        $result = checkAndCompleteLine($line);
        if ($result[1]) {
            $scores[] = $result[0];
        }
    }
}

fclose($file);

sort($scores);
$middleScore = $scores[count($scores) / 2];
echo $middleScore . PHP_EOL;

function checkAndCompleteLine($line) {
    $pairings = [')' => '(', ']' => '[', '}' => '{', '>' => '<'];
    $scoreValues = [')' => 1, ']' => 2, '}' => 3, '>' => 4];
    $opening = "([{<";
    $closing = ")]}>";
    $stack = [];

    for ($i = 0; $i < strlen($line); $i++) {
        $char = $line[$i];
        if (strpos($opening, $char) !== false) {
            array_push($stack, $char);
        } elseif (strpos($closing, $char) !== false) {
            if (empty($stack) || end($stack) != $pairings[$char]) {
                return [0, false]; // corrupted line
            }
            array_pop($stack); // pop from stack
        }
    }

    if (empty($stack)) {
        return [0, false]; // line is not incomplete
    }

    // Calculate score for incomplete line
    $score = 0;
    for ($i = count($stack) - 1; $i >= 0; $i--) {
        $score *= 5;
        $score += $scoreValues[getClosingChar($stack[$i])];
    }
    return [$score, true];
}

function getClosingChar($openingChar) {
    switch ($openingChar) {
        case '(':
            return ')';
        case '[':
            return ']';
        case '{':
            return '}';
        case '<':
            return '>';
        default:
            return ' ';
    }
}
?>
