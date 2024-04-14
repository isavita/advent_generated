<?php
$data = file_get_contents("input.txt");
$lines = explode("\n", trim($data));
$checksum = 0;

foreach ($lines as $line) {
    $nums = preg_split('/\s+/', $line);
    $minVal = PHP_INT_MAX;
    $maxVal = PHP_INT_MIN;

    foreach ($nums as $numStr) {
        $num = intval($numStr);
        if ($num < $minVal) {
            $minVal = $num;
        }
        if ($num > $maxVal) {
            $maxVal = $num;
        }
    }

    $checksum += ($maxVal - $minVal);
}

echo $checksum;
?>