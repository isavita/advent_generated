
<?php

$validCount = 0;
$file = fopen("input.txt", "r") or die("Unable to open file!");

while (!feof($file)) {
    $line = fgets($file);
    $i = strpos($line, ":");
    if ($i !== false) {
        $policy = substr($line, 0, $i);
        $password = substr($line, $i + 2);
        if (validatePassword($policy, $password)) {
            $validCount++;
        }
    }
}

fclose($file);
echo $validCount . PHP_EOL;

function validatePassword($policy, $password) {
    sscanf($policy, "%d-%d %c", $min, $max, $char);
    $count = 0;
    for ($i = 0; $i < strlen($password); $i++) {
        if ($password[$i] === $char) {
            $count++;
        }
    }
    return $count >= $min && $count <= $max;
}
?>
