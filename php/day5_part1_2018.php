<?php
function react($polymer) {
    $stack = [];
    for ($i = 0; $i < strlen($polymer); $i++) {
        if (!empty($stack) && abs(ord($stack[count($stack) - 1]) - ord($polymer[$i])) == 32) {
            array_pop($stack);
        } else {
            $stack[] = $polymer[$i];
        }
    }
    return implode('', $stack);
}

$file = fopen("input.txt", "r");
$polymer = fgets($file);
fclose($file);

$result = react(trim($polymer));
echo strlen($result);
?>