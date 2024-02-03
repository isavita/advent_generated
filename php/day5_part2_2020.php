
<?php

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);
$seatIDs = [];

foreach ($lines as $line) {
    $pass = str_replace(["F", "B", "L", "R"], ["0", "1", "0", "1"], $line);
    $seatID = decode($pass);
    $seatIDs[] = $seatID;
}

sort($seatIDs);

for ($i = 0; $i < count($seatIDs) - 1; $i++) {
    if ($seatIDs[$i + 1] != $seatIDs[$i] + 1) {
        echo $seatIDs[$i] + 1 . PHP_EOL;
        break;
    }
}

function decode($pass) {
    $row = binaryToInt(substr($pass, 0, 7));
    $column = binaryToInt(substr($pass, 7));
    return $row * 8 + $column;
}

function binaryToInt($binaryStr) {
    $result = 0;
    for ($i = 0; $i < strlen($binaryStr); $i++) {
        if ($binaryStr[$i] == '1') {
            $result |= 1 << (strlen($binaryStr) - $i - 1);
        }
    }
    return $result;
}
?>
