
<?php

$data = file_get_contents("input.txt");
$passphrases = explode("\n", trim($data));
$validCount = 0;

foreach ($passphrases as $passphrase) {
    $words = explode(" ", $passphrase);
    $wordSet = [];

    $valid = true;
    foreach ($words as $word) {
        $sortedWord = sortString($word);
        if (isset($wordSet[$sortedWord])) {
            $valid = false;
            break;
        }
        $wordSet[$sortedWord] = true;
    }

    if ($valid) {
        $validCount++;
    }
}

echo $validCount;

function sortString($w) {
    $s = str_split($w);
    sort($s);
    return implode("", $s);
}
?>
