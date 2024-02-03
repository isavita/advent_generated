
<?php

$file = fopen("input.txt", "r");
if ($file === false) {
    echo "Error opening file\n";
    return;
}

$prev = 0;
$count = 0;

while (($line = fgets($file)) !== false) {
    $current = intval($line);
    if ($prev != 0 && $current > $prev) {
        $count++;
    }
    $prev = $current;
}

fclose($file);

echo $count . "\n";
?>
