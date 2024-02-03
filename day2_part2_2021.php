
<?php

$file = fopen("input.txt", "r");
$horizontalPosition = 0;
$depth = 0;
$aim = 0;

while (!feof($file)) {
    $line = fgets($file);
    $command = explode(" ", $line);
    $direction = $command[0];
    $units = (int)$command[1];

    switch ($direction) {
        case "forward":
            $horizontalPosition += $units;
            $depth += $aim * $units;
            break;
        case "down":
            $aim += $units;
            break;
        case "up":
            $aim -= $units;
            break;
    }
}

$product = $horizontalPosition * $depth;
echo $product;

fclose($file);
?>
