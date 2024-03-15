<?php

function generateAddresses($mask, $address) {
    $floating = [];
    $addresses = [];

    // Apply mask '1's and collect floating positions
    for ($i = 0; $i < strlen($mask); $i++) {
        if ($mask[$i] == '1') {
            $address |= (1 << (35 - $i));
        } elseif ($mask[$i] == 'X') {
            $floating[] = 35 - $i;
        }
    }

    // Generate all combinations for the floating bits
    $count = 1 << count($floating);
    for ($i = 0; $i < $count; $i++) {
        $modAddress = $address;
        for ($j = 0; $j < count($floating); $j++) {
            if ($i & (1 << $j)) {
                $modAddress |= (1 << $floating[$j]); // Set bit
            } else {
                $modAddress &= ~(1 << $floating[$j]); // Clear bit
            }
        }
        $addresses[] = $modAddress;
    }
    return $addresses;
}

$file = fopen("input.txt", "r");
$mask = "";
$mem = [];

while (($line = fgets($file)) !== false) {
    $line = trim($line);
    if (strpos($line, "mask = ") === 0) {
        $mask = substr($line, 7);
    } else {
        if (preg_match('/mem\[(\d+)\] = (\d+)/', $line, $matches)) {
            $address = intval($matches[1]);
            $value = intval($matches[2]);
            $addresses = generateAddresses($mask, $address);
            foreach ($addresses as $addr) {
                $mem[$addr] = $value;
            }
        }
    }
}

fclose($file);

$sum = 0;
foreach ($mem as $value) {
    $sum += $value;
}

echo $sum . "\n";
?>