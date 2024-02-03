
<?php

function readInput($filename) {
    $file = fopen($filename, "r");
    $input = fgets($file);
    fclose($file);
    return trim($input);
}

function findNextPassword($password) {
    while (true) {
        $password = incrementPassword($password);
        if (isValidPassword($password)) {
            break;
        }
    }
    return $password;
}

function incrementPassword($password) {
    $password = strrev($password);
    $carry = 1;
    for ($i = 0; $i < strlen($password); $i++) {
        $ascii = ord($password[$i]) + $carry;
        $password[$i] = chr($ascii > ord('z') ? ord('a') : $ascii);
        $carry = $ascii > ord('z') ? 1 : 0;
        if ($carry == 0) {
            break;
        }
    }
    return strrev($password);
}

function isValidPassword($password) {
    return hasStraight($password) && !containsInvalidLetters($password) && hasTwoPairs($password);
}

function hasStraight($password) {
    for ($i = 0; $i < strlen($password) - 2; $i++) {
        if (ord($password[$i]) + 1 == ord($password[$i + 1]) && ord($password[$i]) + 2 == ord($password[$i + 2])) {
            return true;
        }
    }
    return false;
}

function containsInvalidLetters($password) {
    return strpos($password, 'i') !== false || strpos($password, 'o') !== false || strpos($password, 'l') !== false;
}

function hasTwoPairs($password) {
    $count = 0;
    for ($i = 0; $i < strlen($password) - 1; $i++) {
        if ($password[$i] == $password[$i + 1]) {
            $count++;
            $i++;
        }
    }
    return $count >= 2;
}

$currentPassword = readInput("input.txt");
$firstNewPassword = findNextPassword($currentPassword);
$secondNewPassword = findNextPassword($firstNewPassword);
echo $secondNewPassword . PHP_EOL;

?>
