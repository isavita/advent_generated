
<?php

$input = trim(file_get_contents("input.txt"));
$halfway = strlen($input) / 2;
$sum = 0;

for ($i = 0; $i < strlen($input); $i++) {
    $next = ($i + $halfway) % strlen($input);
    if ($input[$i] == $input[$next]) {
        $sum += ord($input[$i]) - ord('0');
    }
}

echo $sum . PHP_EOL;
?>
