<?php

$data = file_get_contents("input.txt");
$lines = explode("\n", trim($data));
$sum = 0;

foreach ($lines as $line) {
    $nums = array_map('intval', preg_split('/\s+/', $line));
    $count = count($nums);
    for ($i = 0; $i < $count; $i++) {
        for ($j = 0; $j < $count; $j++) {
            if ($i != $j && $nums[$i] % $nums[$j] == 0) {
                $sum += $nums[$i] / $nums[$j];
            }
        }
    }
}

echo $sum;
?>