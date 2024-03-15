<?php
$input = trim(file_get_contents('input.txt'));

function findLowestNumber($input, $zeroes) {
    $i = 1;
    while (true) {
        $hash = md5($input . $i);
        if (substr($hash, 0, $zeroes) === str_repeat('0', $zeroes)) {
            return $i;
        }
        $i++;
    }
}

echo "Part 1: " . findLowestNumber($input, 5) . "\n";
echo "Part 2: " . findLowestNumber($input, 6) . "\n";