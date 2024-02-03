
<?php

$input = trim(file_get_contents("input.txt"));
$sum = 0;

for ($i = 0; $i < strlen($input); $i++) {
    $next = ($i + 1) % strlen($input);
    if ($input[$i] == $input[$next]) {
        $sum += (int)$input[$i];
    }
}

echo $sum . PHP_EOL;
?>
