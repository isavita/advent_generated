
<?php

$input = file_get_contents("input.txt");
$x = [1];

foreach (explode("\n", $input) as $line) {
    switch ($line) {
        case "noop":
            $x[] = end($x);
            break;
        default:
            $n = (int) sscanf($line, "addx %d")[0];
            $x[] = end($x);
            $x[] = end($x) + $n;
            break;
    }
}

$sum = 0;
foreach ($x as $i => $value) {
    if (($i - 19) % 40 == 0) {
        $sum += ($i + 1) * $value;
    }
}

echo $sum . PHP_EOL;
?>
