
<?php

$input = file_get_contents("input.txt");
$operations = explode("\n", trim($input));
$password = "abcdefgh";

foreach ($operations as $op) {
    $password = applyOperation($op, $password);
}

echo $password;

function applyOperation($op, $password) {
    $fields = explode(" ", $op);
    switch ($fields[0]) {
        case "swap":
            switch ($fields[1]) {
                case "position":
                    $x = (int)$fields[2];
                    $y = (int)$fields[5];
                    $password = swapPosition($password, $x, $y);
                    break;
                case "letter":
                    $x = $fields[2];
                    $y = $fields[5];
                    $password = swapLetter($password, $x, $y);
                    break;
            }
            break;
        case "rotate":
            switch ($fields[1]) {
                case "left":
                    $steps = (int)$fields[2];
                    $password = rotateLeft($password, $steps);
                    break;
                case "right":
                    $steps = (int)$fields[2];
                    $password = rotateRight($password, $steps);
                    break;
                case "based":
                    $x = $fields[6];
                    $password = rotateBasedOnPosition($password, $x);
                    break;
            }
            break;
        case "reverse":
            $x = (int)$fields[2];
            $y = (int)$fields[4];
            $password = reversePositions($password, $x, $y);
            break;
        case "move":
            $x = (int)$fields[2];
            $y = (int)$fields[5];
            $password = movePosition($password, $x, $y);
            break;
    }
    return $password;
}

function swapPosition($password, $x, $y) {
    $password = str_split($password);
    $temp = $password[$x];
    $password[$x] = $password[$y];
    $password[$y] = $temp;
    return implode("", $password);
}

function swapLetter($password, $x, $y) {
    $password = str_replace($x, "_", $password);
    $password = str_replace($y, $x, $password);
    $password = str_replace("_", $y, $password);
    return $password;
}

function rotateLeft($password, $steps) {
    $steps = $steps % strlen($password);
    return substr($password, $steps) . substr($password, 0, $steps);
}

function rotateRight($password, $steps) {
    $steps = $steps % strlen($password);
    return substr($password, -1 * $steps) . substr($password, 0, -1 * $steps);
}

function rotateBasedOnPosition($password, $x) {
    $index = strpos($password, $x);
    $steps = 1 + $index;
    if ($index >= 4) {
        $steps++;
    }
    return rotateRight($password, $steps);
}

function reversePositions($password, $x, $y) {
    $start = substr($password, 0, $x);
    $middle = strrev(substr($password, $x, $y - $x + 1));
    $end = substr($password, $y + 1);
    return $start . $middle . $end;
}

function movePosition($password, $x, $y) {
    $password = str_split($password);
    $char = $password[$x];
    array_splice($password, $x, 1);
    array_splice($password, $y, 0, $char);
    return implode("", $password);
}
?>
