<?php
$input = file_get_contents('input.txt');
$sum = 0;

function sumNumbers($data) {
    global $sum;
    if (is_array($data)) {
        foreach ($data as $item) {
            sumNumbers($item);
        }
    } elseif (is_object($data)) {
        foreach ($data as $value) {
            sumNumbers($value);
        }
    } elseif (is_numeric($data)) {
        $sum += $data;
    }
}

sumNumbers(json_decode($input));
echo $sum;