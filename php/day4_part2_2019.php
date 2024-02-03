
<?php

$input = file_get_contents("input.txt");
$range = explode("-", trim($input));
$start = (int)$range[0];
$end = (int)$range[1];

$count = 0;
for ($i = $start; $i <= $end; $i++) {
    if (isValidPassword($i)) {
        $count++;
    }
}

echo $count . PHP_EOL;

function isValidPassword($password) {
    $s = (string)$password;
    $hasDouble = false;

    for ($i = 0; $i < strlen($s) - 1; $i++) {
        if ($s[$i] > $s[$i + 1]) {
            return false;
        }
        if ($s[$i] == $s[$i + 1]) {
            if (($i == 0 || $s[$i] != $s[$i - 1]) && ($i + 2 >= strlen($s) || $s[$i] != $s[$i + 2])) {
                $hasDouble = true;
            }
        }
    }

    return $hasDouble;
}
?>
