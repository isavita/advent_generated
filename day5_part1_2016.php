
<?php

$doorID = trim(file_get_contents("input.txt"));
$password = findPassword($doorID);
echo $password . PHP_EOL;

function findPassword($doorID) {
    $password = '';
    $i = 0;
    while (strlen($password) < 8) {
        $hash = md5Hash($doorID . $i);
        if (substr($hash, 0, 5) === "00000") {
            $password .= $hash[5];
        }
        $i++;
    }
    return $password;
}

function md5Hash($input) {
    return md5($input);
}
?>
