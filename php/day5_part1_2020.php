
<?php

$file = fopen("input.txt", "r");
$maxSeatID = 0;

while (!feof($file)) {
    $pass = fgets($file);
    $pass = str_replace(["F", "B", "L", "R"], ["0", "1", "0", "1"], $pass);
    $seatID = decode($pass);
    if ($seatID > $maxSeatID) {
        $maxSeatID = $seatID;
    }
}

echo $maxSeatID;

function decode($pass) {
    $row = bindec(substr($pass, 0, 7));
    $column = bindec(substr($pass, 7));
    return $row * 8 + $column;
}
