
<?php

$file = fopen("input.txt", "r");
$horizontalPosition = 0;
$depth = 0;

while (!feof($file)) {
    $line = fgets($file);
    $command = explode(" ", $line);
    $direction = $command[0];
    $units = (int)$command[1];

    switch ($direction) {
        case "forward":
            $horizontalPosition += $units;
            break;
        case "down":
            $depth += $units;
            break;
        case "up":
            $depth -= $units;
            break;
    }
}

$product = $horizontalPosition * $depth;
echo $product;

fclose($file);
?>
