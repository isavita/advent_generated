<?php
$data = file_get_contents("input.txt");
$jsonData = json_decode($data, true);

function sumNumbers($data) {
    $sum = 0;
    if (is_array($data)) {
        if (array_keys($data) !== range(0, count($data) - 1)) { // associative array
            if (!in_array("red", $data, true)) {
                foreach ($data as $value) {
                    $sum += sumNumbers($value);
                }
            }
        } else { // indexed array
            foreach ($data as $value) {
                $sum += sumNumbers($value);
            }
        }
    } elseif (is_int($data)) {
        $sum += $data;
    }
    return $sum;
}

$sum = sumNumbers($jsonData);
echo $sum;
?>