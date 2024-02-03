
<?php

$file = fopen("input.txt", "r");
$numbers = [];

while (!feof($file)) {
    $line = trim(fgets($file));
    if ($line != "") {
        $numbers[] = intval($line);
    }
}

fclose($file);

for ($i = 0; $i < count($numbers) - 1; $i++) {
    for ($j = $i + 1; $j < count($numbers); $j++) {
        if ($numbers[$i] + $numbers[$j] == 2020) {
            echo $numbers[$i] * $numbers[$j] . "\n";
            exit;
        }
    }
}
?>
