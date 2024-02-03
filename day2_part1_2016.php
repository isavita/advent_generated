
<?php

$file = fopen("input.txt", "r");
$instructions = [];
while (!feof($file)) {
    $instructions[] = trim(fgets($file));
}
fclose($file);

$keypad = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
];
$x = 1;
$y = 1;
$code = '';

foreach ($instructions as $instruction) {
    for ($i = 0; $i < strlen($instruction); $i++) {
        $move = $instruction[$i];
        switch ($move) {
            case 'U':
                if ($x > 0) {
                    $x--;
                }
                break;
            case 'D':
                if ($x < 2) {
                    $x++;
                }
                break;
            case 'L':
                if ($y > 0) {
                    $y--;
                }
                break;
            case 'R':
                if ($y < 2) {
                    $y++;
                }
                break;
        }
    }
    $code .= $keypad[$x][$y];
}

echo $code . "\n";
?>
