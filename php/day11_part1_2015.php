<?php
$input = file_get_contents('input.txt');

function isValid($password) {
    $hasIncreasingStraight = false;
    $hasTwoNonOverlappingPairs = false;
    $hasNoILO = true;

    for ($i = 0; $i < strlen($password) - 2; $i++) {
        if (ord($password[$i]) + 1 == ord($password[$i + 1]) && ord($password[$i + 1]) + 1 == ord($password[$i + 2])) {
            $hasIncreasingStraight = true;
            break;
        }
    }

    $pairs = [];
    for ($i = 0; $i < strlen($password) - 1; $i++) {
        if ($password[$i] == $password[$i + 1]) {
            $pairs[] = $password[$i] . $password[$i + 1];
            if (count(array_unique($pairs)) >= 2) {
                $hasTwoNonOverlappingPairs = true;
                break;
            }
        }
    }

    for ($i = 0; $i < strlen($password); $i++) {
        if ($password[$i] == 'i' || $password[$i] == 'o' || $password[$i] == 'l') {
            $hasNoILO = false;
            break;
        }
    }

    return $hasIncreasingStraight && $hasTwoNonOverlappingPairs && $hasNoILO;
}

function nextPassword($password) {
    while (true) {
        $password++;
        if (isValid($password)) {
            return $password;
        }
    }
}

echo nextPassword($input);
?>