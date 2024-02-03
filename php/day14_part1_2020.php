
<?php

$handle = fopen("input.txt", "r");
if ($handle) {
    $mask = "";
    $mem = [];

    while (($line = fgets($handle)) !== false) {
        if (strpos($line, "mask = ") === 0) {
            $mask = substr($line, 7);
        } else {
            preg_match('/mem\[(\d+)] = (\d+)/', $line, $matches);
            if (!empty($matches)) {
                $address = intval($matches[1]);
                $value = intval($matches[2]);
                $mem[$address] = applyMask($value, $mask);
            }
        }
    }

    $sum = array_sum($mem);
    echo $sum . "\n";

    fclose($handle);
} else {
    echo "Error opening file\n";
}

function applyMask($value, $mask) {
    $result = 0;
    for ($i = 0; $i < 36; $i++) {
        $bitValue = 1 << (35 - $i);
        if ($mask[$i] == '1') {
            $result |= $bitValue;
        } elseif ($mask[$i] == 'X') {
            $result |= ($value & $bitValue);
        }
    }
    return $result;
}
