
<?php

$input = file_get_contents("input.txt");
$steps = (int)trim($input);
$buffer = [0];
$currentPos = 0;

for ($i = 1; $i <= 2017; $i++) {
    $currentPos = ($currentPos + $steps) % count($buffer);
    array_splice($buffer, $currentPos + 1, 0, $i);
    $currentPos++;
}

foreach ($buffer as $key => $val) {
    if ($val == 2017) {
        echo $buffer[($key + 1) % count($buffer)] . PHP_EOL;
        break;
    }
}
?>
