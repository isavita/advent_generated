
<?php

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);
$cardPublicKey = intval($lines[0]);
$doorPublicKey = intval($lines[1]);

function transform($subjectNumber, $loopSize) {
    $value = 1;
    for ($i = 0; $i < $loopSize; $i++) {
        $value *= $subjectNumber;
        $value %= 20201227;
    }
    return $value;
}

function findLoopSize($publicKey) {
    $value = 1;
    $loopSize = 0;
    while ($value != $publicKey) {
        $value *= 7;
        $value %= 20201227;
        $loopSize++;
    }
    return $loopSize;
}

$cardLoopSize = findLoopSize($cardPublicKey);
$encryptionKey = transform($doorPublicKey, $cardLoopSize);

echo $encryptionKey . "\n";
?>
