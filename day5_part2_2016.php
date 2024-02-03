
<?php

$doorID = trim(file_get_contents("input.txt"));
$password = findPassword($doorID);
echo $password . PHP_EOL;

function findPassword($doorID) {
    $password = str_repeat(" ", 8);
    $filledPositions = 0;
    $found = array_fill(0, 8, false);

    for ($i = 0; $filledPositions < 8; $i++) {
        $hash = md5Hash($doorID . $i);
        if (substr($hash, 0, 5) === "00000") {
            $pos = $hash[5];
            if ($pos >= '0' && $pos <= '7') {
                $posIndex = (int)$pos;
                if (!$found[$posIndex]) {
                    $found[$posIndex] = true;
                    $password[$posIndex] = $hash[6];
                    $filledPositions++;
                }
            }
        }
    }
    return $password;
}

function md5Hash($input) {
    return md5($input);
}
?>
