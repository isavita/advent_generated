
<?php

$data = file_get_contents("input.txt");
$salt = trim($data);
$keys = 0;
$index = 0;

while ($keys < 64) {
    $hash = md5($salt . $index);
    $triplet = findTriplet($hash);
    
    if ($triplet !== "") {
        for ($i = 1; $i <= 1000; $i++) {
            $nextHash = md5($salt . ($index + $i));
            if (strpos($nextHash, str_repeat($triplet, 5)) !== false) {
                $keys++;
                break;
            }
        }
    }
    
    $index++;
}

echo $index - 1 . PHP_EOL;

function findTriplet($hash) {
    for ($i = 0; $i < strlen($hash) - 2; $i++) {
        if ($hash[$i] === $hash[$i + 1] && $hash[$i] === $hash[$i + 2]) {
            return $hash[$i];
        }
    }
    return "";
}
?>
