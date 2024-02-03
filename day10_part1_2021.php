
<?php

$file = fopen("input.txt", "r") or die("Unable to open file!");
$totalScore = 0;

while (!feof($file)) {
    $line = trim(fgets($file));
    $result = checkLine($line);
    if ($result[1]) {
        $totalScore += $result[0];
    }
}

fclose($file);
echo $totalScore;

function checkLine($line) {
    $pairings = [')' => '(', ']' => '[', '}' => '{', '>' => '<'];
    $scores = [')' => 3, ']' => 57, '}' => 1197, '>' => 25137];
    $stack = [];

    for ($i = 0; $i < strlen($line); $i++) {
        $char = $line[$i];
        switch ($char) {
            case '(':
            case '[':
            case '{':
            case '<':
                array_push($stack, $char);
                break;
            case ')':
            case ']':
            case '}':
            case '>':
                if (empty($stack) || end($stack) != $pairings[$char]) {
                    return [$scores[$char], true]; // corrupted line
                }
                array_pop($stack); // pop from stack
                break;
        }
    }
    return [0, false]; // line is not corrupted
}
?>
