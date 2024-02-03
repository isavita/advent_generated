
<?php

$hashCache = [];

$data = file_get_contents("input.txt");
$salt = trim($data);
$keys = 0;
$index = 0;

while ($keys < 64) {
    $hash = getStretchedMD5Hash($salt . $index);
    $triplet = findTriplet($hash);
    if ($triplet !== "") {
        for ($i = 1; $i <= 1000; $i++) {
            $nextHash = getStretchedMD5Hash($salt . ($index + $i));
            if (strpos($nextHash, str_repeat($triplet, 5)) !== false) {
                $keys++;
                break;
            }
        }
    }
    $index++;
}

echo $index - 1 . PHP_EOL;

function getStretchedMD5Hash($input) {
    global $hashCache;
    if (array_key_exists($input, $hashCache)) {
        return $hashCache[$input];
    }
    $hash = getMD5Hash($input);
    for ($i = 0; $i < 2016; $i++) {
        $hash = getMD5Hash($hash);
    }
    $hashCache[$input] = $hash;
    return $hash;
}

function getMD5Hash($input) {
    return md5($input);
}

function findTriplet($hash) {
    for ($i = 0; $i < strlen($hash) - 2; $i++) {
        if ($hash[$i] === $hash[$i + 1] && $hash[$i] === $hash[$i + 2]) {
            return $hash[$i];
        }
    }
    return "";
}
