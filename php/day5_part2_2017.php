
<?php

$data = file_get_contents("input.txt");
$lines = explode("\n", trim($data));
$offsets = [];

foreach ($lines as $line) {
    $offset = (int)$line;
    $offsets[] = $offset;
}

$index = 0;
$steps = 0;

while ($index >= 0 && $index < count($offsets)) {
    $jump = $offsets[$index];

    if ($jump >= 3) {
        $offsets[$index]--;
    } else {
        $offsets[$index]++;
    }

    $index += $jump;
    $steps++;
}

echo $steps . "\n";
?>
