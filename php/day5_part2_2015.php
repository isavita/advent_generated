
<?php

$input = trim(file_get_contents("input.txt"));

$nice = 0;
$passesRule1 = function($line) {
    for ($i = 0; $i < strlen($line) - 2; $i++) {
        $toMatch = substr($line, $i, 2);
        for ($j = $i + 2; $j < strlen($line) - 1; $j++) {
            if (substr($line, $j, 2) == $toMatch) {
                return true;
            }
        }
    }
    return false;
};

foreach (explode("\n", $input) as $line) {
    $rule1 = $passesRule1($line);

    $rule2 = false;
    for ($i = 0; $i < strlen($line) - 2; $i++) {
        if ($line[$i] == $line[$i + 2]) {
            $rule2 = true;
            break;
        }
    }
    if ($rule1 && $rule2) {
        $nice++;
    }
}

echo $nice . PHP_EOL;
?>
