
<?php

$input = trim(file_get_contents("input.txt"));
$floor = 0;
for ($i = 0; $i < strlen($input); $i++) {
    if ($input[$i] == '(') {
        $floor++;
    } else if ($input[$i] == ')') {
        $floor--;
    }
}
echo $floor . PHP_EOL;
