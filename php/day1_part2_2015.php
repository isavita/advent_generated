
<?php

$input = trim(file_get_contents("input.txt"));
$floor = 0;
$position = 0;

for ($i = 0; $i < strlen($input); $i++) {
    $c = $input[$i];
    if ($c == '(') {
        $floor++;
    } else if ($c == ')') {
        $floor--;
    }
    if ($floor == -1) {
        $position = $i + 1;
        break;
    }
}

echo $position . PHP_EOL;
?>
